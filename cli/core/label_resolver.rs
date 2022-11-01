use super::Event;
use super::*;
use dashmap::DashMap;
use futures::Future;
use futures::FutureExt;
use fxhash::*;
use std::path::PathBuf;
use std::{pin::Pin, sync::Arc};
use thiserror::*;
use tracing::*;

#[derive(Debug)]
pub struct LabelResolver {
    build_opts: BuildOpts,
    dependency_resolver: DependencyResolver,
    label_registry: Arc<LabelRegistry>,
    remote_workspace_resolver: Arc<RemoteWorkspaceResolver>,
    resolved_labels: DashMap<LabelId, Target>,
    toolchain_configs: FxHashMap<String, RuleConfig>,
    source_manager: Arc<SourceManager>,
    signature_store: Arc<SignatureStore>,
    event_channel: Arc<EventChannel>,
    workspace: Workspace,
    artifact_store: Arc<ArtifactStore>,
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

    #[error("Could not find target: {0:?}")]
    TargetNotFound(Label),

    #[error(transparent)]
    RemoteWorkspaceResolverError(RemoteWorkspaceResolverError),

    #[error(transparent)]
    DependencyResolverError(DependencyResolverError),

    #[error(transparent)]
    SignatureError(SignatureError),
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

        Self {
            artifact_store,
            build_opts,
            dependency_resolver,
            event_channel,
            label_registry,
            remote_workspace_resolver,
            resolved_labels: DashMap::default(),
            signature_store,
            source_manager,
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

                match self.find_in_remote_workspaces(&label).await {
                    Ok(Some(target)) => {
                        self.save(label_id, target.clone());
                        return Ok(target);
                    }
                    Ok(None) => (),
                    Err(err) => {
                        if let Some(target) = self.find_with_dependency_resolver(label_id).await? {
                            self.save(label_id, target.clone());
                            return Ok(target);
                        }
                        return Err(err);
                    }
                }
            }

            if label.is_file() {
                let target = self.find_as_file(label_id, &label).await?;
                self.save(label_id, target.clone());
                return Ok(target);
            }

            if label.is_abstract() {
                if let Some(target) = self.find_in_local_workspace(&label).await? {
                    self.save(label_id, target.clone());
                    return Ok(target);
                }
            }

            Err(LabelResolverError::TargetNotFound((*label).clone()))
        }
        .boxed_local()
    }

    #[tracing::instrument(name = "LabelResolver::save", skip(self))]
    fn save(&self, label: LabelId, target: Target) {
        self.resolved_labels.insert(label, target);
    }

    #[tracing::instrument(name = "LabelResolver::find_in_local_workspace", skip(self))]
    async fn find_in_local_workspace(
        &self,
        label: &Label,
    ) -> Result<Option<Target>, LabelResolverError> {
        // Try to find a Signature file for this Label.
        let abstract_label = label.get_abstract().unwrap();
        let buildfile_path = abstract_label
            .workspace()
            .join(&abstract_label)
            .join(BUILDFILE);

        let buildfile = SignaturesFile::read(buildfile_path, abstract_label.workspace())
            .await
            .map_err(LabelResolverError::SignatureError)?;

        let target: Option<Target> = buildfile
            .signatures
            .iter()
            .find(|s| s.name.name() == *label.name())
            .cloned()
            .map(|s| s.into());

        Ok(target)
    }

    #[tracing::instrument(name = "LabelResolver::find_as_toolchain", skip(self))]
    async fn find_as_toolchain(&self, label: &Label) -> Result<Option<Target>, LabelResolverError> {
        if let Some(config) = self.toolchain_configs.get(&label.name().to_string()) {
            let target = Target::new(
                label.to_local(&self.workspace).unwrap(),
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

    #[tracing::instrument(name = "LabelResolver::find_in_remote_workspaces", skip(self))]
    async fn find_in_remote_workspaces(
        &self,
        label: &Label,
    ) -> Result<Option<Target>, LabelResolverError> {
        let target = self
            .remote_workspace_resolver
            .get(label)
            .await
            .map_err(LabelResolverError::RemoteWorkspaceResolverError)?;

        Ok(target)
    }

    #[tracing::instrument(name = "LabelResolver::find_with_dependency_resolver", skip(self))]
    async fn find_with_dependency_resolver(
        &self,
        label_id: LabelId,
    ) -> Result<Option<Target>, LabelResolverError> {
        let target = self
            .dependency_resolver
            .get(label_id)
            .await
            .map_err(LabelResolverError::DependencyResolverError)?;

        Ok(target)
    }

    #[tracing::instrument(name = "LabelResolver::find_as_file", skip(self))]
    async fn find_as_file(
        &self,
        label_id: LabelId,
        label: &Label,
    ) -> Result<Target, LabelResolverError> {
        self.event_channel.send(Event::GeneratingSignature {
            label: label.to_owned(),
        });

        // 1. Register this label as a source file, and store its AST -- at this stage we will be
        //    using tree-sitter to provide us with an very fast and accurate enough representation
        //    of this program.
        //
        let source = self
            .source_manager
            .register_source(label_id, label)
            .await
            .unwrap();

        // 2. Figure out what inside that AST we care about -- we need to know exactly which
        //    signatures we are interested in, and since a source file can have more than one, we
        //    generate a SourceSymbol by inspecting the label and the current goal.
        //
        let symbol = SourceSymbol::from_label_and_goal(label_id, label, self.build_opts.goal);

        // 3. Use the symbol to split the source to the subtree we want to generate a signature
        //    for.
        //
        let source_chunk = self
            .source_manager
            .get_source_chunk_by_symbol(source, &symbol)
            .await
            .unwrap() // chunker error
            .unwrap(); // nothing found

        // 4. Generate a signature for this symbol and this source chunk. This signature will
        //    include all the information we need to build/test/run this chunk.
        //
        let signature = self
            .signature_store
            .generate_signature(label_id, label, &source_chunk, symbol)
            .await
            .unwrap();

        Ok(signature.into())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tokio::fs;
    use tokio::io::AsyncWriteExt;

    fn root() -> PathBuf {
        PathBuf::from(&env!("CARGO_MANIFEST_DIR")).join("tests")
    }

    async fn write_workspace() {
        let path = root().join("workspace").join(WORKSPACE);
        let buildfile = r#"
[workspace]
name = "test-workspace"
        "#
        .to_string();

        fs::create_dir_all(path.parent().unwrap()).await.unwrap();
        let mut file = fs::File::create(path).await.unwrap();
        let _ = file.write(buildfile.as_bytes()).await.unwrap();
    }

    async fn write_buildfile(path: PathBuf) {
        let path = root().join("workspace").join(path).join(BUILDFILE);
        let buildfile = r#"
[[rule1]]
name = "pkg"
        "#
        .to_string();

        fs::create_dir_all(path.parent().unwrap()).await.unwrap();
        let mut file = fs::File::create(path).await.unwrap();
        let _ = file.write(buildfile.as_bytes()).await.unwrap();
    }

    async fn resolver() -> LabelResolver {
        let paths = WorkspacePaths::new(
            &root().join("workspace"),
            Some(root().to_str().unwrap().to_string()),
            "test-user".to_string(),
        )
        .unwrap();

        let workspace_file = WorkspaceFile::builder()
            .workspace(
                WorkspaceConfigFile::builder()
                    .name("test-workspace".to_string())
                    .build()
                    .unwrap(),
            )
            .build()
            .unwrap();

        let workspace = Workspace::builder()
            .current_user("test-user".to_string())
            .paths(paths)
            .from_file(workspace_file)
            .await
            .unwrap()
            .build()
            .unwrap();

        LabelResolver::new(&workspace)
    }

    #[tokio::test]
    async fn label_resolver_finds_local_targets() {
        let label = Label::new("//hello/world:pkg");
        write_workspace().await;
        write_buildfile(label.path()).await;
        let resolver = resolver().await;
        let target = resolver.resolve(&label).await.unwrap();
        assert_eq!(label.name(), target.label.name());
    }
}
