use super::*;
use dashmap::DashMap;
use std::path::PathBuf;
use std::sync::Arc;
use thiserror::*;
use tokio::fs;
use tracing::*;

#[derive(Error, Debug)]
pub enum RemoteWorkspaceResolverError {
    #[error("Can't resolve a workspace with a URL that has no host: {0:?}")]
    UrlHadNoHost(url::Url),

    #[error(r#"Remote workspace for host {0} needs to be configured in the Workspace.toml - you can do that by adding this:

[remote_workspaces]
{0} = {{ url = "https://{0}/path/to/archive.tar.gz", sha256 = ".." }}

or

[remote_workspaces]
{0} = {{ github = "{{username}}/{{repo}}", ref = "git-commit-sha" }}

"#)]
    MissingConfig(String),

    #[error("Could not create a download lock for remote workspace {} at {path:?}, due to: {err:?} - config: {config:?}", config.url().to_string())]
    LockCreationError {
        path: PathBuf,
        err: std::io::Error,
        config: RemoteWorkspaceConfig,
    },

    #[error(transparent)]
    ArchiveManagerError(ArchiveManagerError),
}

#[derive(Debug)]
pub struct RemoteWorkspaceResolver {
    root_workspace: Workspace,
    remote_workspace_configs: DashMap<String, RemoteWorkspaceConfig>,
    global_workspaces_path: PathBuf,
    archive_manager: ArchiveManager,
    workspaces: DashMap<String, Workspace>,
    targets: DashMap<Label, Target>,
    store: Arc<Store>,
}

impl RemoteWorkspaceResolver {
    #[tracing::instrument(name = "RemoteWorkspaceResolver::new", skip(workspace))]
    pub fn new(workspace: &Workspace, store: Arc<Store>, event_channel: Arc<EventChannel>) -> Self {
        Self {
            root_workspace: workspace.clone(),
            remote_workspace_configs: {
                let map = DashMap::default();
                for (k, v) in workspace.remote_workspace_configs.iter() {
                    map.insert(k.clone(), v.clone());
                }
                map
            },
            global_workspaces_path: workspace.paths.global_workspaces_path.clone(),
            targets: DashMap::new(),
            workspaces: DashMap::new(),
            archive_manager: ArchiveManager::new(workspace, event_channel),
            store,
        }
    }

    #[tracing::instrument(name = "RemoteWorkspaceResolver::register", skip(self))]
    pub fn register(&self, config: RemoteWorkspaceConfig) {
        let host = config.url().host().unwrap().to_string();
        self.remote_workspace_configs.insert(host, config);
    }

    #[tracing::instrument(name = "RemoteWorkspaceResolver::get", skip(self))]
    pub async fn get(&self, label: &Label) -> Result<Option<Target>, RemoteWorkspaceResolverError> {
        if let Some(entry) = self.targets.get(label) {
            let target = entry.value().clone();
            return Ok(Some(target));
        }

        let mut url = label.url();
        url.set_path("");

        let workspace = self.find_workspace(label).await?;

        let label = label.change_workspace(&workspace);

        let buildfile = Buildfile::from_label(&label)
            .await
            .map_err(LabelResolverError::BuildfileError)
            .unwrap();

        let target = buildfile
            .targets
            .iter()
            .find(|t| t.label.name() == *label.name())
            .map(|t| t.change_workspace(&workspace).with_associated_url(&url));

        if let Some(ref target) = target {
            self.targets.insert(label.clone(), target.clone());
        }

        Ok(target)
    }

    fn _store_path(&self, config: &RemoteWorkspaceConfig) -> PathBuf {
        self.global_workspaces_path.join(config.path())
    }

    fn _warp_lock_path(&self, config: &RemoteWorkspaceConfig) -> PathBuf {
        self._store_path(config).join("Warp.lock")
    }

    #[tracing::instrument(name = "RemoteWorkspaceResolver::find_workspace", skip(self))]
    async fn find_workspace(
        &self,
        label: &Label,
    ) -> Result<Workspace, RemoteWorkspaceResolverError> {
        let host = label
            .url()
            .host()
            .ok_or_else(|| RemoteWorkspaceResolverError::UrlHadNoHost(label.url()))?
            .to_string();

        if let Some(workspace) = self.workspaces.get(&host) {
            Ok(workspace.clone())
        } else if let Some(config) = self.remote_workspace_configs.get(&host) {
            self.ensure_workspace(&config).await?;
            // NOTE(@ostera): once we know that we have a workspace ready in this
            // folder, we can use the current label and _reparent it_ to use the
            // path to this workspace.
            let label_path = format!(".{}", label.url().path());
            let workspace_path = self._store_path(&config);

            let relative_label_path = workspace_path.join(&label_path);

            let (root, workspace_file) = WorkspaceFile::find_upwards(&relative_label_path)
                .await
                .unwrap();

            let current_user = self.root_workspace.current_user.clone();
            let paths = WorkspacePaths::new(&root, None, current_user.clone()).unwrap();

            let workspace = Workspace::builder()
                .current_user(current_user)
                .paths(paths)
                .from_file(workspace_file)
                .await
                .unwrap()
                .build()
                .unwrap();

            self.store.register_workspace(&workspace);

            self.workspaces.insert(host.clone(), workspace.clone());

            Ok(workspace)
        } else {
            Err(RemoteWorkspaceResolverError::MissingConfig(
                host.to_string(),
            ))
        }
    }

    #[tracing::instrument(name = "RemoteWorkspaceResolver::ensure_workspace", skip(self))]
    async fn ensure_workspace(
        &self,
        config: &RemoteWorkspaceConfig,
    ) -> Result<(), RemoteWorkspaceResolverError> {
        // NOTE(@ostera): if the lock file is there, we assume its already downloaded
        // TODO(@ostera): move the lock functionality into the ArchiveManager
        if self.lock_exists(config).await {
            return Ok(());
        }

        let url = config.url();

        let final_dir = self._store_path(config);

        let expected_hash = if config.is_github() {
            None
        } else {
            Some(config.hash().to_string())
        };

        let strip_prefix = Some(config.prefix().to_string());

        self.archive_manager
            .download_and_extract(url, &final_dir, expected_hash, strip_prefix)
            .await
            .map_err(RemoteWorkspaceResolverError::ArchiveManagerError)?;

        self.create_lock(config).await?;

        Ok(())
    }

    #[tracing::instrument(name = "RemoteWorkspaceResolver::lock_exists", skip(self))]
    async fn lock_exists(&self, config: &RemoteWorkspaceConfig) -> bool {
        let warp_lock = self._warp_lock_path(config);
        fs::metadata(&warp_lock).await.is_ok()
    }

    #[tracing::instrument(name = "RemoteWorkspaceResolver::create_lock", skip(self))]
    async fn create_lock(
        &self,
        config: &RemoteWorkspaceConfig,
    ) -> Result<(), RemoteWorkspaceResolverError> {
        let warp_lock = self._warp_lock_path(config);
        fs::File::create(&warp_lock)
            .await
            .map(|_| ())
            .map_err(|err| RemoteWorkspaceResolverError::LockCreationError {
                config: config.clone(),
                path: warp_lock,
                err,
            })
    }
}
