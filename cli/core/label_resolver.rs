use super::*;
use dashmap::DashMap;
use futures::Future;
use futures::FutureExt;
use fxhash::*;
use std::path::PathBuf;
use std::{pin::Pin, sync::Arc};
use thiserror::*;
use tokio::fs;
use tracing::*;

#[derive(Debug)]
pub struct LabelResolver {
    build_opts: BuildOpts,
    dependency_resolver: DependencyResolver,
    label_registry: Arc<LabelRegistry>,
    remote_workspace_resolver: Arc<RemoteWorkspaceResolver>,
    resolved_labels: DashMap<LabelId, Target>,
    toolchain_configs: FxHashMap<String, RuleConfig>,
    event_channel: Arc<EventChannel>,
    workspace: Workspace,
    artifact_store: Arc<ArtifactStore>,
    source_resolver: SourceResolver,
}

#[derive(Error, Debug)]
pub enum LabelResolverError {
    #[error(r#"Remote label {:?} needs to be configured in the Workspace.toml - you can do that by adding this:

[toolchains.{}]
sha1 = "fixme"
version = "9.1.4"

And try running the command again to see what the right `sha1` should be.

        "#, .0.to_string(), .0.name())]
    RemoteLabelNeedsConfig(Label),

    #[error("Could not find target: {}", .0.to_string())]
    TargetNotFound(Label),

    #[error(transparent)]
    RemoteWorkspaceResolverError(RemoteWorkspaceResolverError),

    #[error(transparent)]
    DependencyResolverError(DependencyResolverError),

    #[error(transparent)]
    SignatureError(SignatureError),

    #[error(transparent)]
    SourceManagerError(SourceManagerError),

    #[error(transparent)]
    SignatureStoreError(SignatureStoreError),
}

impl LabelResolver {
    pub fn new(
        workspace: &Workspace,
        artifact_store: Arc<ArtifactStore>,
        build_results: Arc<BuildResults>,
        event_channel: Arc<EventChannel>,
        label_registry: Arc<LabelRegistry>,
        dependency_manager: Arc<DependencyManager>,
        source_manager: Arc<SourceManager>,
        signature_store: Arc<SignatureStore>,
        build_opts: BuildOpts,
    ) -> Self {
        let remote_workspace_resolver = Arc::new(RemoteWorkspaceResolver::new(
            workspace,
            artifact_store.clone(),
            event_channel.clone(),
        ));

        let dependency_resolver = DependencyResolver::new(
            workspace,
            build_results,
            event_channel.clone(),
            artifact_store.clone(),
            label_registry.clone(),
            dependency_manager,
        );

        let source_resolver = SourceResolver::new(
            event_channel.clone(),
            label_registry.clone(),
            source_manager,
            signature_store,
            build_opts,
        );

        Self {
            artifact_store,
            build_opts,
            dependency_resolver,
            event_channel,
            label_registry,
            remote_workspace_resolver,
            resolved_labels: DashMap::default(),
            source_resolver,
            toolchain_configs: workspace.toolchain_configs.clone(),
            workspace: workspace.clone(),
        }
    }

    #[tracing::instrument(name = "LabelResolver::resolve", skip(self))]
    pub fn resolve<'a>(
        &'a self,
        label_id: LabelId,
    ) -> Pin<Box<dyn Future<Output = Result<Target, LabelResolverError>> + 'a>> {
        async move {
            if let Some(target) = self.resolved_labels.get(&label_id) {
                return Ok(target.value().clone());
            }

            let label = self.label_registry.get_label(label_id);

            if label.is_remote() {
                if let Some(target) = self.find_as_toolchain(&label).await? {
                    self.save(label_id, target.clone());
                    return Ok(target);
                }

                match self
                    .remote_workspace_resolver
                    .resolve(label_id, &label)
                    .await
                {
                    Ok(Some(target)) => {
                        self.save(label_id, target.clone());
                        return Ok(target);
                    }
                    Ok(None) => (),
                    Err(
                        err @ LabelResolverError::RemoteWorkspaceResolverError(
                            RemoteWorkspaceResolverError::SignatureError(_),
                        ),
                    ) => return Err(err),
                    Err(err) => {
                        if let Some(target) =
                            self.dependency_resolver.resolve(label_id, &label).await?
                        {
                            self.save(label_id, target.clone());
                            return Ok(target);
                        }
                        return Err(err);
                    }
                }
            } else if label.is_abstract() {
                if let Some(target) = self.find_in_local_workspace(&label).await? {
                    self.save(label_id, target.clone());
                    return Ok(target);
                }
            } else if label.is_file() {
                let target = self.source_resolver.resolve(label_id, &label).await?;
                self.save(label_id, target.clone());
                return Ok(target);
            }

            Err(LabelResolverError::TargetNotFound((*label).clone()))
        }
        .boxed_local()
    }

    #[tracing::instrument(name = "LabelResolver::save", skip(self))]
    fn save(&self, label: LabelId, target: Target) {
        self.resolved_labels.insert(label, target);
    }

    /// Finds a Target by its AbstractLabel in the Current Workspace.
    ///
    #[tracing::instrument(name = "LabelResolver::find_in_local_workspace", skip(self))]
    async fn find_in_local_workspace(
        &self,
        label: &Label,
    ) -> Result<Option<Target>, LabelResolverError> {
        // 1. Make sure we have an abstract label first. We want this because we expect this label
        //    to be pointing a target within a Build.json file.
        //
        let abstract_label = label.get_abstract().unwrap();

        // 2. But we want to work with a label to a local file, so we consume it into one.
        //
        let local_label: LocalLabel = abstract_label.to_owned().into();

        // 3. Now we can read the Build.json with all the signatures.
        //
        let buildfile_path = local_label.workspace().join(&local_label).join(BUILDFILE);
        let buildfile = SignaturesFile::read(buildfile_path, local_label.workspace())
            .await
            .map_err(LabelResolverError::SignatureError)?;

        let mut target: Option<Target> = buildfile
            .signatures
            .into_iter()
            .find(|s| s.name.name() == *label.name())
            .map(|s| s.into());

        if let Some(target) = &mut target {
            // 4. If we find a specific target, we'll make sure to mark its dependencies as either
            //    LocalLabels (if they point to files) or AbstractLabels (if they point to a dir
            //    with a Build.json file)).
            //
            let workspace_root = local_label.workspace();
            for dep in target.deps.iter_mut().chain(target.runtime_deps.iter_mut()) {
                if dep.is_remote() {
                    continue;
                }

                if let Ok(meta) = fs::metadata(dep.path()).await {
                    if meta.is_file() {
                        *dep = dep.to_local(&workspace_root).unwrap()
                    } else {
                        *dep = dep.to_abstract().unwrap()
                    }
                }
            }

            // 5. Finally, since the target will have a label that is _abstract_, we will want to
            //    turn it into a LocalLabel that is ready for building.
            //
            target.label = local_label.into();
        }

        Ok(target)
    }

    /// Find a RemoteLabel as a Toolchain.
    ///
    #[tracing::instrument(name = "LabelResolver::find_as_toolchain", skip(self))]
    async fn find_as_toolchain(&self, label: &Label) -> Result<Option<Target>, LabelResolverError> {
        let remote_label = label.get_remote().unwrap();

        // NOTE(@ostera): this is a *HACK* because we want to use the name of the toolchain (say
        // `erlang`) but the label for it is a URL (say
        // `https://rules.warp.build/toolchains/erlang`).
        //
        // We *DO NOT* want this hack to go into the RemoteLabel, since RemoteLabels can be
        // legitimately used for building everything within a file, or a specific target within it.
        //
        // For example, say we want to build `https://my.app/lib/a.go`. This URL turned into a
        // label means: "fetch, analyze, and build all the signatures generated for a.go".
        //
        // The `name` of the label would be `None`.
        //
        // However, if we wanted to run a single test within it, we'd use the syntax
        // `https://my.app/lib/a.go:testName`.
        //
        // The `name` of the label would be `Some("testName")`.
        //
        // And we do not want this meaning to be overloaded here.
        //
        let name = remote_label
            .path
            .file_name()
            .unwrap()
            .to_string_lossy()
            .to_string();

        if let Some(config) = self.toolchain_configs.get(&name) {
            let target = Target::new(
                label
                    .to_local(&self.workspace.paths.workspace_root)
                    .unwrap(),
                &label.get_remote().unwrap().url_str(),
                config.clone(),
            );

            // NOTE(@ostera): if a label was successfully lifted
            if let Some(local) = target.label.get_local() {
                if let Some(remote) = local.promoted_from() {
                    self.artifact_store.register_workspace_raw(
                        local.workspace().to_path_buf(),
                        PathBuf::from(&remote.prefix_hash),
                    );
                };
            };

            Ok(Some(target))
        } else {
            Ok(None)
        }
    }
}
