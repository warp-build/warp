use super::*;
use crate::sync::*;
use crate::Config;
use dashmap::DashMap;
use thiserror::*;
use url::Url;

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
    local_workspaces: DashMap<WorkspaceId, Arc<Workspace>>,

    remote_workspaces: DashMap<WorkspaceId, Arc<Workspace>>,

    registered_urls: DashMap<Url, WorkspaceId>,

    remote_workspace_root: PathBuf,

    config: Config,

    _register_lock: Arc<Mutex<()>>,
}

impl WorkspaceManager {
    pub fn new(config: Config) -> Self {
        Self {
            remote_workspace_root: config.remote_workspace_root().to_path_buf(),
            current_workspace: Default::default(),
            local_workspaces: Default::default(),
            remote_workspaces: Default::default(),
            registered_urls: Default::default(),
            _register_lock: Arc::new(Mutex::new(())),
            config,
        }
    }

    /// Get the current workspace. This function MUST BE called after calling
    /// `load_current_workspace`. Otherwise it will panic.
    ///
    pub fn current_workspace(&self) -> Arc<Workspace> {
        let cw_id = *self.current_workspace.read().unwrap();
        self.local_workspaces
            .get(&cw_id)
            .map(|e| e.value().clone())
            .expect("We tried to access the current workspace but it wasn't loaded! This is a bug.")
    }

    /// Find the a workspace using the `WorkspaceFinder`, and registers it as the current
    /// workspace.
    ///
    pub async fn load_current_workspace(&self) -> Result<WorkspaceId, WorkspaceManagerError> {
        let workspace = WorkspaceFinder::find(&self.config).await?;

        let current_workspace_id = self.register_local_workspace(workspace)?;
        self.set_current_workspace(current_workspace_id);

        Ok(current_workspace_id)
    }

    pub fn register_local_workspace(
        &self,
        w: Workspace,
    ) -> Result<WorkspaceId, WorkspaceManagerError> {
        let id = WorkspaceId::next();
        self.local_workspaces.insert(id, w.into());
        Ok(id)
    }

    pub fn register_remote_workspace(&self, url: &Url) -> WorkspaceId {
        let _lock = self._register_lock.lock().unwrap();

        if let Some(id) = self.find_remote_workspace(url) {
            id
        } else {
            let id = WorkspaceId::next();

            let url = url.to_string();
            let root = self.remote_workspace_root.join(url.replace("://", "/"));
            let w = Workspace::builder().name(url).root(root).build().unwrap();

            self.remote_workspaces.insert(id, w.into());
            id
        }
    }

    pub fn find_remote_workspace(&self, url: &Url) -> Option<WorkspaceId> {
        self.registered_urls.get(url).map(|r| *r.value())
    }

    pub fn get_workspace(&self, id: WorkspaceId) -> Arc<Workspace> {
        if let Some(workspace) = self.local_workspaces.get(&id) {
            return workspace.clone();
        }

        if let Some(workspace) = self.remote_workspaces.get(&id) {
            return workspace.clone();
        }

        unreachable!()
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

        let wm = WorkspaceManager::new(config.clone());
        let _wid = wm.load_current_workspace().await.unwrap();

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

        let wm = WorkspaceManager::new(config.clone());
        let _wid = wm.load_current_workspace().await.unwrap();

        assert_eq!(
            wm.current_workspace().root(),
            &curr_workspace.path().to_path_buf()
        );
        assert_eq!(wm.current_workspace().name(), "test_from_subdir");
    }
}
