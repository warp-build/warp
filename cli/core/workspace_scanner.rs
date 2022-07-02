use super::*;
use anyhow::Context;
use futures::FutureExt;
use futures::StreamExt;
use std::path::PathBuf;
use tokio::fs;
use tracing::*;

/// A struct to scan Workspace paths to find different files.
pub struct WorkspaceScanner {
    /// The paths used to scan.
    paths: WorkspacePaths,

    /// Paths to ignore during scans.
    ignore_patterns: Vec<String>,
}

impl WorkspaceScanner {
    pub fn from_workspace(workspace: &Workspace) -> WorkspaceScanner {
        WorkspaceScanner {
            paths: workspace.paths.clone(),
            ignore_patterns: workspace.ignore_patterns.clone(),
        }
    }

    pub fn find_workspace_file(
        cwd: &PathBuf,
    ) -> futures::future::BoxFuture<'_, Result<(PathBuf, PathBuf), anyhow::Error>> {
        async move {
            let here = &cwd.join(WORKSPACE);
            debug!("Searching for workspace file in {:?}", here);
            if fs::metadata(here).await.is_ok() {
                debug!("Found it! Continuing...");
                let root = fs::canonicalize(here.parent().unwrap()).await?;
                Ok((root.to_path_buf(), here.to_path_buf()))
            } else {
                let parent = cwd.parent().context(
                    "Reached the top of the file system and could not find a workspace file",
                )?;
                WorkspaceScanner::find_workspace_file(&parent.to_path_buf()).await
            }
        }
        .boxed()
    }

    pub async fn find_build_files(
        &self,
        max_concurrency: usize,
    ) -> Result<impl futures::Stream<Item = Result<PathBuf, anyhow::Error>>, anyhow::Error> {
        debug!(
            "Scanning for build files in {:?}",
            self.paths.workspace_root
        );
        let paths = FileScanner::new()
            .max_concurrency(max_concurrency)
            .matching_path(WARPFILE)?
            .starting_from(&self.paths.workspace_root)
            .await?
            .skipping_paths(&self.ignore_patterns)?
            .stream_files()
            .await;

        Ok(Box::pin(paths))
    }

    pub async fn find_rules(&self) -> Result<Vec<PathBuf>, anyhow::Error> {
        debug!(
            "Scanning for local rules in {:?}",
            self.paths.local_rules_root
        );
        let mut files = Box::pin(
            FileScanner::new()
                .starting_from(&self.paths.local_rules_root)
                .await?
                .matching_path("\\.js$")?
                .stream_files()
                .await,
        );

        let mut paths = vec![];
        while let Some(path) = files.next().await {
            paths.push(path?.clone());
        }

        debug!("Found {} local rules...", paths.len());

        Ok(paths)
    }

    pub async fn find_toolchains(&self) -> Result<Vec<PathBuf>, anyhow::Error> {
        debug!(
            "Scanning for local toolchains in {:?}",
            self.paths.local_toolchains_root
        );

        let mut files = Box::pin(
            FileScanner::new()
                .starting_from(&self.paths.local_toolchains_root)
                .await?
                .matching_path("\\.js$")?
                .stream_files()
                .await,
        );

        let mut paths = vec![];
        while let Some(path) = files.next().await {
            paths.push(path?.clone());
        }

        debug!("Found {} local toolchains...", paths.len());

        Ok(paths)
    }
}
