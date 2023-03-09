use super::*;
use crate::sync::{Arc, RwLock};
use crate::Config;
use dashmap::DashMap;
use thiserror::*;

/// # Find, register, and access different workspaces.
///
/// The workspace manager helps keep track of the different kinds of workspaces that we have in
/// Warp, the workspaces that have been registered during execution, and how to access them.
///
/// Workspaces help Warp understand what are the boundaries between repositories/projects, and how
/// to find things in the cache.
///
#[derive(Default, Debug, Clone)]
pub struct WorkspaceManager {
    /// The WorkspaceId of the current workspace.
    ///
    current_workspace: Arc<RwLock<WorkspaceId>>,

    /// A map of all the local workspaces, registered by their id.
    ///
    /// NOTE(@ostera): in the future we may wrap this with more metadata that is specific for local
    /// workspaces.
    local_workspaces: DashMap<WorkspaceId, Workspace>,
}

impl WorkspaceManager {
    pub fn new() -> Self {
        Self::default()
    }

    /// Get the current workspace. This function MUST BE called after calling
    /// `load_current_workspace`. Otherwise it will panic.
    ///
    pub fn current_workspace(&self) -> Workspace {
        let cw_id = *self.current_workspace.read().unwrap();
        self.local_workspaces
            .get(&cw_id)
            .map(|e| e.value().clone())
            .expect("We tried to access the current workspace but it wasn't loaded! This is a bug.")
    }

    /// Find the a workspace using the `WorkspaceFinder`, and registers it as the current
    /// workspace.
    ///
    pub async fn load_current_workspace(
        &self,
        config: &Config,
    ) -> Result<WorkspaceId, WorkspaceManagerError> {
        let workspace = WorkspaceFinder::find(config).await?;

        let current_workspace_id = self.register_local_workspace(workspace)?;
        self.set_current_workspace(current_workspace_id);

        Ok(current_workspace_id)
    }

    pub fn register_local_workspace(
        &self,
        w: Workspace,
    ) -> Result<WorkspaceId, WorkspaceManagerError> {
        let id = WorkspaceId::next();
        self.local_workspaces.insert(id, w);
        Ok(id)
    }

    pub fn set_current_workspace(&self, current_workspace_id: WorkspaceId) {
        let mut cw = self.current_workspace.write().unwrap();
        *cw = current_workspace_id;
    }
}

#[derive(Error, Debug)]
pub enum WorkspaceManagerError {
    #[error(transparent)]
    WorkspaceFinderError(WorkspaceFinderError),
}

impl From<WorkspaceFinderError> for WorkspaceManagerError {
    fn from(err: WorkspaceFinderError) -> Self {
        Self::WorkspaceFinderError(err)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::WARPFILE;
    use assert_fs::prelude::*;

    #[tokio::test]
    async fn loading_current_workspace_at_the_root() {
        let curr_workspace = assert_fs::TempDir::new().unwrap();

        let warpfile = curr_workspace.child(WARPFILE);
        warpfile
            .write_str(
                r#"
        {
            "workspace": {
                "name": "test-from-root"
            }
        }
        "#,
            )
            .unwrap();

        let config = Config::builder()
            .invocation_dir(curr_workspace.path().to_path_buf())
            .build()
            .unwrap();

        let wm = WorkspaceManager::new();
        let _wid = wm.load_current_workspace(&config).await.unwrap();

        assert_eq!(
            wm.current_workspace().root(),
            &curr_workspace.path().to_path_buf()
        );
        assert_eq!(wm.current_workspace().name(), "test-from-root");
    }

    #[tokio::test]
    async fn loading_current_workspace_from_subdirs() {
        let curr_workspace = assert_fs::TempDir::new().unwrap();

        let warpfile = curr_workspace.child(WARPFILE);
        warpfile
            .write_str(
                r#"
        {
            "workspace": {
                "name": "test_from_subdir"
            }
        }
        "#,
            )
            .unwrap();

        let subdir = curr_workspace.child("some/deep/sub/folder");
        subdir.create_dir_all().unwrap();

        let config = Config::builder()
            .invocation_dir(subdir.path().to_path_buf())
            .build()
            .unwrap();

        let wm = WorkspaceManager::new();
        let _wid = wm.load_current_workspace(&config).await.unwrap();

        assert_eq!(
            wm.current_workspace().root(),
            &curr_workspace.path().to_path_buf()
        );
        assert_eq!(wm.current_workspace().name(), "test_from_subdir");
    }
}
