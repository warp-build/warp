use super::*;
use crate::{Config, WarpConfigFile, WarpConfigFileError, WARPFILE};
use futures::StreamExt;
use std::path::{Path, PathBuf};
use thiserror::*;
use tokio::fs;
use tracing::*;

pub struct WorkspaceFinder;

/// The WorkspaceFinder implements the heuristics for finding a workspace from an invocation
/// directory.
///
/// This allows us to run `warp` anywhere within a `Workspace` and still find it.
///
impl WorkspaceFinder {
    pub async fn find(config: &Config) -> Result<Workspace, WorkspaceFinderError> {
        let (root, warpfile) = Self::find_upwards(config.invocation_dir()).await?;

        let workspace = Workspace::builder()
            .root(root)
            .name(warpfile.workspace.name)
            .build()?;

        Ok(workspace)
    }

    #[tracing::instrument(name = "WorkspaceFile::find_upwards")]
    async fn find_upwards(cwd: &Path) -> Result<(PathBuf, WarpConfigFile), WorkspaceFinderError> {
        let mut dirs = Box::pin(Self::walk_uptree(cwd.to_path_buf()).await);
        while let Some(path) = dirs.next().await {
            let here = &path.join(WARPFILE);
            if fs::metadata(&here).await.is_ok() {
                let root = path.canonicalize().unwrap();
                let warpfile = WarpConfigFile::read(here).await?;
                return Ok((root, warpfile));
            }
        }
        Err(WorkspaceFinderError::WorkspaceFileNotFound)
    }

    #[tracing::instrument(name = "WorkspaceFile::walk_uptree")]
    async fn walk_uptree(start: PathBuf) -> impl futures::Stream<Item = PathBuf> {
        let mut cwd = start;
        async_stream::stream! {
            yield cwd.clone();
            while let Some(parent) = cwd.parent() {
                cwd = parent.to_path_buf();
                yield cwd.clone();
            }
        }
    }
}

#[derive(Error, Debug)]
pub enum WorkspaceFinderError {
    #[error(transparent)]
    WarpConfigFileError(WarpConfigFileError),

    #[error(transparent)]
    WorkspacePathsError(WorkspacePathsError),

    #[error(transparent)]
    WorkspaceError(WorkspaceError),

    #[error("Could not find a workspace while walking upwards from the current directory. Are you sure we are in a warp-capable project?")]
    WorkspaceFileNotFound,
}

impl From<WarpConfigFileError> for WorkspaceFinderError {
    fn from(err: WarpConfigFileError) -> Self {
        Self::WarpConfigFileError(err)
    }
}

impl From<WorkspacePathsError> for WorkspaceFinderError {
    fn from(err: WorkspacePathsError) -> Self {
        Self::WorkspacePathsError(err)
    }
}

impl From<WorkspaceError> for WorkspaceFinderError {
    fn from(err: WorkspaceError) -> Self {
        Self::WorkspaceError(err)
    }
}

#[cfg(test)]
mod tests {}
