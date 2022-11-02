use super::Event;
use super::*;
use dashmap::DashMap;
use std::{path::PathBuf, sync::Arc};
use thiserror::*;
use tokio::fs;
use tokio::io::AsyncReadExt;
use tracing::*;
use url::Url;

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

    #[error("Could not parse Dependency Resolution contents: \n\n{resolution}\n\ndue to {err:#?}")]
    ParseError {
        err: serde_json::Error,
        resolution: String,
    },

    #[error(transparent)]
    ExtractionError(ArchiveManagerError),
}

#[derive(Debug)]
pub struct DependencyResolver {
    global_workspaces_path: PathBuf,

    targets: DashMap<LabelId, Target>,
    resolvers: DashMap<String, LabelId>,

    dependency_manager: Arc<DependencyManager>,

    build_results: Arc<BuildResults>,
    label_registry: Arc<LabelRegistry>,
    event_channel: Arc<EventChannel>,
    artifact_store: Arc<ArtifactStore>,
    archive_manager: ArchiveManager,
}

impl DependencyResolver {
    #[tracing::instrument(name = "DependencyResolver::new", skip(workspace))]
    pub fn new(
        workspace: &Workspace,
        build_results: Arc<BuildResults>,
        event_channel: Arc<EventChannel>,
        artifact_store: Arc<ArtifactStore>,
        label_registry: Arc<LabelRegistry>,
        dependency_manager: Arc<DependencyManager>,
    ) -> Self {
        let this = Self {
            archive_manager: ArchiveManager::new(workspace, event_channel.clone()),
            build_results,
            artifact_store,
            targets: DashMap::new(),
            resolvers: DashMap::new(),
            event_channel,
            dependency_manager,
            global_workspaces_path: workspace.paths.global_workspaces_path.clone(),
            label_registry,
        };

        // TODO(@ostera): these should come from a registry, like the toolchains registry
        for (host, resolver) in [
            ("hex.pm", "https://tools.warp.build/hexpm/resolver"),
            ("github.com", "https://tools.warp.build/github/resolver"),
            ("gitlab.com", "https://tools.warp.build/gitlab/resolver"),
            ("npmjs.com", "https://tools.warp.build/npm/resolver"),
        ] {
            let label: Label = Url::parse(resolver).unwrap().into();
            let label_id = this.label_registry.register_label(label);
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

        let label = self.label_registry.get_label(label_id);
        let remote_label = label.get_remote().unwrap();

        let dependency = self.dependency_manager.get(label_id).ok_or_else(|| {
            DependencyResolverError::UnspecifiedDependency {
                dependency: remote_label.to_owned().into(),
            }
        })?;

        if let Some(resolver) = self.resolvers.get(&remote_label.host) {
            if let Some(target) = self
                .resolve(*resolver, label_id, remote_label, dependency)
                .await?
            {
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
        remote_label: &RemoteLabel,
        dependency: Dependency,
    ) -> Result<Option<Target>, DependencyResolverError> {
        // 1. build resolver
        let main_entry = self
            .build_results
            .get_build_result(resolver)
            .ok_or(DependencyResolverError::MissingResolver { resolver })?;

        // 1.1. if the dependency actually needs a separate resolver for analyzing the contents,
        //   lets make sure its ready.
        let BuildResult {
            target_manifest: override_manifest,
            executable_target: override_target,
        } = if let Some(override_resolver) = dependency.resolver {
            self.build_results
                .get_build_result(override_resolver)
                .ok_or(DependencyResolverError::MissingResolver {
                    resolver: override_resolver,
                })?
        } else {
            main_entry.clone()
        };

        let BuildResult {
            target_manifest: manifest,
            executable_target: target,
        } = main_entry;

        // 2. Resolve dependency

        // 2.1. create a workspace for this dependency

        // https://hex.pm/packages/proper -> become a workspace
        // let mut dep_workspace = DependencyWorkspace::from_label(label);
        let url = remote_label.url_str().replace("://", "/");

        // WorkspaceManager.prepare_workspace(&mut dep_workspace).await?;
        let final_dir = self
            .global_workspaces_path
            .join(url)
            .join(&dependency.version);
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

        self.artifact_store
            .register_workspace_raw(final_dir.clone(), PathBuf::from(workspace_name));

        // -- interlude --
        // try to fetch the resolution file from disk
        let signature_file_path = final_dir.join("Warp.signature");

        let resolution = if fs::metadata(&signature_file_path).await.is_ok() {
            let mut signature_file = fs::File::open(signature_file_path).await.unwrap();
            let mut bytes = vec![];
            signature_file.read_to_end(&mut bytes).await.unwrap();
            serde_json::from_slice(&bytes).unwrap()
        } else {
            self.event_channel.send(Event::ResolvingDependency {
                label: remote_label.to_owned().into(),
            });
            // 2.2. actually run the resolver to get the workspace contents
            let cmd = CommandRunner::builder()
                .cwd(final_dir.clone())
                .manifest(manifest.clone())
                .target(target.clone())
                .args(vec![
                    "resolve".to_string(),
                    dependency.url.to_string(),
                    dependency.version.clone(),
                    dependency.package.clone(),
                ])
                .sandboxed(true)
                .stream_outputs(false)
                .build()
                .map_err(DependencyResolverError::CommandRunnerError)?;

            let str_output = cmd
                .run()
                .await
                .map_err(DependencyResolverError::CommandRunnerError)?;

            let resolution: DependencyArchiveResolution =
                serde_json::from_slice(str_output.as_bytes()).map_err(|err| {
                    DependencyResolverError::ParseError {
                        err,
                        resolution: str_output.clone(),
                    }
                })?;

            // 2.3. download the workspace contents
            self.archive_manager
                .download_and_extract(
                    &resolution.archive.url,
                    &final_dir,
                    None,
                    resolution.archive.strip_prefix,
                )
                .await
                .map_err(DependencyResolverError::ExtractionError)?;

            // 3. let the resolver tell us how to build this workspace
            let cmd = CommandRunner::builder()
                .cwd(final_dir.clone())
                .manifest(override_manifest)
                .target(override_target)
                .args(vec![
                    "prepare".to_string(),
                    final_dir.to_str().unwrap().to_string(),
                    dependency.url.to_string(),
                    dependency.version.clone(),
                    dependency.package.clone(),
                ])
                .sandboxed(false)
                .stream_outputs(false)
                .build()
                .map_err(DependencyResolverError::CommandRunnerError)?;

            let str_output = cmd
                .run()
                .await
                .map_err(DependencyResolverError::CommandRunnerError)?;

            // parse it
            let resolution: GeneratedSignature = serde_json::from_slice(str_output.as_bytes())
                .map_err(|err| DependencyResolverError::ParseError {
                    err,
                    resolution: str_output.clone(),
                })?;

            let json = serde_json::to_string_pretty(&resolution).unwrap();
            fs::write(final_dir.join("Warp.signature"), json)
                .await
                .unwrap();

            resolution
        };

        // 4. Turn our new signature into a Target
        let sig = resolution.signatures[0].clone();
        let mut target: Target = sig.into();

        // 5. The Target itself will have a label that is not pointing to the right workspace
        //    (since it will be considered "local" to itself, but we are in a different workspace).
        //
        //    To fix this, we will turn it into a LocalLabel and set the workspace to the current
        //    dependency workspace.
        //
        target.label = target.label.to_local(&final_dir).unwrap();
        target.label.set_associated_url(remote_label.url());
        /*
        for dep in target.deps.iter_mut().chain(target.runtime_deps.iter_mut()) {
            *dep = {
                let mut d = dep.to_abstract().unwrap();
                d.set_workspace(&final_dir);
                d
            };
        }
        */

        Ok(Some(target))
    }
}
