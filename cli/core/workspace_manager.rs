use super::*;
use std::path::PathBuf;
use thiserror::*;
use tokio::fs;
use tracing::*;

/// The Workspace Manager takes care of registering workspaces, ensuring they exist, and preparing
/// the directory structure to set them up in different ways.
///
#[derive(Debug, Clone)]
pub struct WorkspaceManager {
    global_workspaces_path: PathBuf,
}

pub trait WorkspaceConfig: std::fmt::Debug {
    fn path(&self) -> PathBuf;
}

#[derive(Error, Debug)]
pub enum WorkspaceManagerError {
    #[error("Could not ensure workspace at {path:?} due to: {err:?}")]
    FileError { path: PathBuf, err: std::io::Error },

    #[error("Could not create a download lock for workspace at {path:?}, due to: {err:?}")]
    LockCreationError { path: PathBuf, err: std::io::Error },
}

impl WorkspaceManager {
    pub fn new(workspace: &Workspace) -> Self {
        Self {
            global_workspaces_path: workspace.paths.global_workspaces_path.clone(),
        }
    }

    pub async fn prepare_workspace(
        &self,
        config: &impl WorkspaceConfig,
    ) -> Result<(), WorkspaceManagerError> {
        if self.lock_exists(config).await {
            return Ok(());
        }

        let final_dir = self._store_path(config);
        fs::create_dir_all(&final_dir)
            .await
            .map_err(|err| WorkspaceManagerError::FileError {
                path: final_dir.clone(),
                err,
            })?;

        Ok(())
    }

    #[tracing::instrument(name = "WorkspaceManager::lock_exists", skip(self, config))]
    async fn lock_exists(&self, config: &impl WorkspaceConfig) -> bool {
        let warp_lock = self._warp_lock_path(config);
        fs::metadata(&warp_lock).await.is_ok()
    }

    #[tracing::instrument(name = "WorkspaceManager::create_lock", skip(self, config))]
    async fn create_lock(
        &self,
        config: &impl WorkspaceConfig,
    ) -> Result<(), WorkspaceManagerError> {
        let warp_lock = self._warp_lock_path(config);
        fs::File::create(&warp_lock)
            .await
            .map(|_| ())
            .map_err(|err| WorkspaceManagerError::LockCreationError {
                path: warp_lock,
                err,
            })
    }

    fn _store_path(&self, config: &impl WorkspaceConfig) -> PathBuf {
        self.global_workspaces_path.join(config.path())
    }

    fn _warp_lock_path(&self, config: &impl WorkspaceConfig) -> PathBuf {
        self._store_path(config).join("Warp.lock")
    }
}

trait WorkspaceT {
    fn config(&self) -> Box<dyn WorkspaceConfig>;
}

#[derive(Debug, Clone)]
struct DependencyWorkspace {}

#[derive(Error, Debug)]
pub enum DependencyWorkspaceError {
    #[error("Could not create dependency workspace {file:?} due to: {err:?}")]
    FileError { file: PathBuf, err: std::io::Error },
}
