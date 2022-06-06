use super::*;
use anyhow::Context;
use futures::FutureExt;
use std::path::PathBuf;
use tokio::fs;
use tracing::*;

/// A struct to scan Workspace paths to find different files.
pub struct WorkspaceScanner {
    /// The paths used to scan.
    paths: WorkspacePaths,
}

impl WorkspaceScanner {
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

    pub fn from_paths(paths: &WorkspacePaths) -> WorkspaceScanner {
        WorkspaceScanner {
            paths: paths.clone(),
        }
    }

    pub async fn find_build_files(
        &self,
        max_concurrency: usize,
    ) -> Result<Vec<PathBuf>, anyhow::Error> {
        debug!(
            "Scanning for build files in {:?}",
            self.paths.workspace_root
        );
        let paths = FileScanner::new()
            .max_concurrency(max_concurrency)
            .matching_path(ZAPFILE)?
            .starting_from(&self.paths.workspace_root)
            .skipping_paths(&["\\.git", "_build", "deps", "lib/bs", "target"])?
            .find_files()
            .await?;

        debug!("Found {} build files...", paths.len());

        Ok(paths)
    }

    pub async fn find_rules(&self) -> Result<Vec<PathBuf>, anyhow::Error> {
        debug!(
            "Scanning for local rules in {:?}",
            self.paths.local_rules_root
        );
        let paths = FileScanner::new()
            .starting_from(&self.paths.local_rules_root)
            .matching_path("\\.js$")?
            .find_files()
            .await?;

        debug!("Found {} local rules...", paths.len());

        Ok(paths)
    }

    pub async fn find_toolchains(&self) -> Result<Vec<PathBuf>, anyhow::Error> {
        debug!(
            "Scanning for local toolchains in {:?}",
            self.paths.local_toolchains_root
        );

        let paths = FileScanner::new()
            .starting_from(&self.paths.local_toolchains_root)
            .matching_path("\\.js$")?
            .find_files()
            .await?;

        debug!("Found {} local toolchains...", paths.len());

        Ok(paths)
    }
}
