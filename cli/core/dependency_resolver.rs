use super::Event;
use super::*;
use dashmap::DashMap;
use serde::{Deserialize, Serialize};
use std::{collections::BTreeMap, path::PathBuf, sync::Arc};
use thiserror::*;
use tokio::fs;
use tracing::*;

#[derive(Error, Debug)]
pub enum DependencyResolverError {
    #[error("Can't resolve a dependency with a URL that has no host: {0:?}")]
    UrlHadNoHost(url::Url),

    #[error(transparent)]
    ArchiveManagerError(ArchiveManagerError),

    #[error(transparent)]
    CommandRunnerError(CommandRunnerError),

    #[error("We don't yet have resolver built!")]
    MissingResolver { resolver: LabelId },

    #[error("Dependency {} has no version in the Workspace.json", .dependency.to_string())]
    UnspecifiedDependency { dependency: Label },
}

#[derive(Debug)]
pub struct DependencyResolver {
    global_workspaces_path: PathBuf,

    targets: DashMap<LabelId, Target>,
    resolvers: DashMap<String, LabelId>,

    dependency_map: BTreeMap<String, String>,

    build_results: Arc<BuildResults>,
    label_registry: Arc<LabelRegistry>,
    event_channel: Arc<EventChannel>,
    store: Arc<Store>,
    archive_manager: ArchiveManager,

    remote_workspace_resolver: Arc<RemoteWorkspaceResolver>,
}

impl DependencyResolver {
    #[tracing::instrument(name = "DependencyResolver::new", skip(workspace))]
    pub fn new(
        workspace: &Workspace,
        build_results: Arc<BuildResults>,
        event_channel: Arc<EventChannel>,
        remote_workspace_resolver: Arc<RemoteWorkspaceResolver>,
        store: Arc<Store>,
        label_registry: Arc<LabelRegistry>,
    ) -> Self {
        let this = Self {
            archive_manager: ArchiveManager::new(workspace, event_channel.clone()),
            build_results,
            store,
            targets: DashMap::new(),
            resolvers: DashMap::new(),
            event_channel,
            dependency_map: workspace.dependency_map.clone(),
            remote_workspace_resolver,
            global_workspaces_path: workspace.paths.global_workspaces_path.clone(),
            label_registry,
        };

        // TODO(@ostera): these should come from a registry, like the toolchains registry
        for (host, resolver) in [
            ("hex.pm", "https://tools.warp.build/hexpm/resolver"),
            ("github.com", "https://tools.warp.build/github/resolver"),
            ("npmjs.com", "https://tools.warp.build/npm/resolver"),
        ] {
            let label = Label::new(resolver);
            let label_id = this.label_registry.register(label);
            this.resolvers.insert(host.to_string(), label_id);
        }

        this
    }

    #[tracing::instrument(name = "DependencyResolver::get", skip(self))]
    pub async fn get(&self, label_id: LabelId) -> Result<Option<Target>, DependencyResolverError> {
        if let Some(entry) = self.targets.get(&label_id) {
            let target = entry.value().clone();
            return Ok(Some(target));
        }

        let label = self.label_registry.get(label_id);
        let version = if let Some(version) = self.dependency_map.get(label.url().as_ref()) {
            version.to_string()
        } else {
            return Err(DependencyResolverError::UnspecifiedDependency { dependency: label });
        };

        let host = label.url().host().unwrap().to_string();
        if let Some(resolver) = self.resolvers.get(&host) {
            if let Some(target) = self.resolve(*resolver, label_id, version).await? {
                self.targets.insert(label_id, target.clone());
                return Ok(Some(target));
            }
        }

        Ok(None)
    }

    #[tracing::instrument(name = "DependencyResolver::resolve", skip(self))]
    async fn resolve(
        &self,
        resolver: LabelId,
        label: LabelId,
        version: String,
    ) -> Result<Option<Target>, DependencyResolverError> {
        // 1. build resolver
        let (manifest, target) = self
            .build_results
            .get_computed_target(resolver)
            .ok_or(DependencyResolverError::MissingResolver { resolver })?;

        let label = self.label_registry.get(label);

        // 2. Resolve dependency
        self.event_channel.send(Event::ResolvingDependency {
            label: label.clone(),
        });

        // 2.1. create a workspace for this dependency

        // https://hex.pm/packages/proper -> become a workspace
        // let mut dep_workspace = DependencyWorkspace::from_label(label);
        let url = label.url().to_string().replace("://", "/");

        // WorkspaceManager.prepare_workspace(&mut dep_workspace).await?;
        let final_dir = self.global_workspaces_path.join(url).join(&version);
        fs::create_dir_all(&final_dir).await.unwrap();

        let workspace_name = {
            use sha2::{Digest, Sha256};
            let mut hasher = Sha256::new();
            hasher.update(final_dir.to_str().unwrap());
            format!(
                "{:x}-{}",
                hasher.finalize(),
                final_dir.file_name().unwrap().to_str().unwrap(),
            )
        };

        self.store
            .register_workspace_raw(final_dir.clone(), PathBuf::from(workspace_name));

        // 2.2. actually run the resolver to get the workspace contents
        let cmd = CommandRunner::builder()
            .cwd(final_dir.clone())
            .manifest(manifest.clone())
            .target(target.clone())
            .args(vec!["resolve".to_string(), label.to_string(), version])
            .sandboxed(true)
            .stream_outputs(true)
            .build()
            .map_err(DependencyResolverError::CommandRunnerError)?;

        let str_output = cmd
            .run()
            .await
            .map_err(DependencyResolverError::CommandRunnerError)?;

        let resolution: DependencyArchiveResolution =
            serde_json::from_slice(str_output.as_bytes()).unwrap();

        // 2.3. download the workspace contents
        self.archive_manager
            .download_and_extract(&resolution.archive.url, &final_dir, None, None)
            .await
            .unwrap();

        // 3. let the resolver tell us how to build this workspace
        let cmd = CommandRunner::builder()
            .cwd(final_dir.clone())
            .manifest(manifest)
            .target(target)
            .args(vec!["prepare".to_string()])
            .sandboxed(false)
            .stream_outputs(true)
            .build()
            .map_err(DependencyResolverError::CommandRunnerError)?;

        let str_output = cmd
            .run()
            .await
            .map_err(DependencyResolverError::CommandRunnerError)?;

        // parse it
        let resolution: DependencySignaturesResolution =
            serde_json::from_slice(str_output.as_bytes()).unwrap();

        // 4. Turn our new signature into a Target
        let sig = resolution.signatures[0].clone();
        let sig_label = Label::builder()
            .name(sig.name.name())
            .workspace(final_dir.to_str().unwrap().to_string())
            .from_path(sig.name.path())
            .unwrap();

        let mut config = sig.config.clone();
        config.insert("name".to_string(), CfgValue::Label(sig_label.clone()));
        config.insert(
            "deps".to_string(),
            CfgValue::List(
                sig.deps
                    .iter()
                    .map(|l| CfgValue::Label(l.clone()))
                    .collect(),
            ),
        );
        let target = Target::new(sig_label, &sig.rule, config);

        Ok(Some(target))
    }
}

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
struct DependencyWorkspace {
    label: Label,
}

#[derive(Error, Debug)]
pub enum DependencyWorkspaceError {
    #[error("Could not create dependency workspace {file:?} due to: {err:?}")]
    FileError { file: PathBuf, err: std::io::Error },
}

#[derive(Builder, Debug, Clone, Serialize, Deserialize)]
pub struct Signature {
    name: Label,

    rule: RuleName,

    #[serde(default)]
    deps: Vec<Label>,

    #[serde(flatten)]
    config: RuleConfig,
}

#[derive(Builder, Debug, Clone, Serialize, Deserialize)]
pub struct Archive {
    url: url::Url,

    #[serde(default)]
    checksum: String,

    #[serde(default)]
    prefix: String,
}

impl From<Archive> for RemoteWorkspaceConfig {
    fn from(a: Archive) -> Self {
        Self::UrlWorkspace {
            url: a.url,
            sha1: a.checksum,
            prefix: a.prefix,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DependencyArchiveResolution {
    #[serde(default)]
    version: usize,

    archive: Archive,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DependencySignaturesResolution {
    #[serde(default)]
    version: usize,

    #[serde(default)]
    signatures: Vec<Signature>,
}
