use super::*;
use futures::StreamExt;
use std::path::PathBuf;
use thiserror::*;
use tracing::*;

#[derive(Error, Debug)]
pub enum WorkspaceScannerError {
    #[error(transparent)]
    FileScannerError(FileScannerError),
}

/// A struct to scan Workspace paths to find different files.
pub struct WorkspaceScanner {
    /// The paths used to scan.
    paths: WorkspacePaths,

    /// Paths to ignore during scans.
    ignore_patterns: Vec<String>,
}

impl WorkspaceScanner {
    pub fn new(paths: &WorkspacePaths, patterns: &[String]) -> WorkspaceScanner {
        WorkspaceScanner {
            paths: paths.clone(),
            ignore_patterns: patterns.to_vec(),
        }
    }

    pub async fn find_build_files(
        &self,
        max_concurrency: usize,
    ) -> Result<impl futures::Stream<Item = Result<PathBuf, FileScannerError>>, WorkspaceScannerError>
    {
        debug!(
            "Scanning for build files in {:?}",
            self.paths.workspace_root
        );
        let paths = FileScanner::new()
            .max_concurrency(max_concurrency)
            .matching_path(WARPFILE)
            .map_err(WorkspaceScannerError::FileScannerError)?
            .starting_from(&self.paths.workspace_root)
            .await
            .map_err(WorkspaceScannerError::FileScannerError)?
            .skipping_paths(&self.ignore_patterns)
            .map_err(WorkspaceScannerError::FileScannerError)?
            .stream_files()
            .await;

        Ok(Box::pin(paths))
    }

    pub async fn find_rules(&self) -> Result<Vec<PathBuf>, WorkspaceScannerError> {
        debug!(
            "Scanning for local rules in {:?}",
            self.paths.local_rules_root
        );
        let mut files = Box::pin(
            FileScanner::new()
                .starting_from(&self.paths.local_rules_root)
                .await
                .map_err(WorkspaceScannerError::FileScannerError)?
                .matching_path("\\.js$")
                .map_err(WorkspaceScannerError::FileScannerError)?
                .skipping_paths(&self.ignore_patterns)
                .map_err(WorkspaceScannerError::FileScannerError)?
                .stream_files()
                .await,
        );

        let mut paths = vec![];
        while let Some(path) = files.next().await {
            let path = path.map_err(WorkspaceScannerError::FileScannerError)?;
            paths.push(path.clone());
        }

        debug!("Found {} local rules...", paths.len());

        Ok(paths)
    }

    pub async fn find_toolchains(&self) -> Result<Vec<PathBuf>, WorkspaceScannerError> {
        debug!(
            "Scanning for local toolchains in {:?}",
            self.paths.local_toolchains_root
        );

        let mut files = Box::pin(
            FileScanner::new()
                .starting_from(&self.paths.local_toolchains_root)
                .await
                .map_err(WorkspaceScannerError::FileScannerError)?
                .matching_path("\\.js$")
                .map_err(WorkspaceScannerError::FileScannerError)?
                .skipping_paths(&self.ignore_patterns)
                .map_err(WorkspaceScannerError::FileScannerError)?
                .stream_files()
                .await,
        );

        let mut paths = vec![];
        while let Some(path) = files.next().await {
            let path = path.map_err(WorkspaceScannerError::FileScannerError)?;
            paths.push(path.clone());
        }

        debug!("Found {} local toolchains...", paths.len());

        Ok(paths)
    }
}
