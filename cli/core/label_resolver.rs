use super::*;
use dashmap::DashMap;
use futures::Future;
use futures::FutureExt;
use fxhash::*;
use std::{pin::Pin, sync::Arc};
use thiserror::*;
use tracing::*;

#[derive(Debug)]
pub struct LabelResolver {
    resolved_labels: DashMap<LabelId, Target>,
    toolchain_configs: FxHashMap<String, RuleConfig>,
    remote_workspace_resolver: Arc<RemoteWorkspaceResolver>,
    dependency_resolver: DependencyResolver,

    label_registry: Arc<LabelRegistry>,
}

#[derive(Error, Debug)]
pub enum LabelResolverError {
    #[error(transparent)]
    BuildfileError(BuildfileError),

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
}

impl LabelResolver {
    pub fn new(
        workspace: &Workspace,
        store: Arc<Store>,
        build_results: Arc<BuildResults>,
        event_channel: Arc<EventChannel>,
        label_registry: Arc<LabelRegistry>,
        dependency_manager: Arc<DependencyManager>,
    ) -> Self {
        let remote_workspace_resolver = Arc::new(RemoteWorkspaceResolver::new(
            workspace,
            store.clone(),
            event_channel.clone(),
        ));
        Self {
            toolchain_configs: workspace.toolchain_configs.clone(),
            dependency_resolver: DependencyResolver::new(
                workspace,
                build_results,
                event_channel,
                store,
                label_registry.clone(),
                dependency_manager,
            ),
            remote_workspace_resolver,
            resolved_labels: DashMap::default(),
            label_registry,
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
            } else if let Some(target) = self.find_in_local_workspace(&label).await? {
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

    #[tracing::instrument(name = "LabelResolver::find_in_local_workspace", skip(self))]
    async fn find_in_local_workspace(
        &self,
        label: &Label,
    ) -> Result<Option<Target>, LabelResolverError> {
        let buildfile = Buildfile::from_label(label)
            .await
            .map_err(LabelResolverError::BuildfileError)?;

        let target = buildfile
            .targets
            .iter()
            .find(|t| t.label.name() == *label.name())
            .cloned();

        Ok(target)
    }

    #[tracing::instrument(name = "LabelResolver::find_as_toolchain", skip(self))]
    async fn find_as_toolchain(&self, label: &Label) -> Result<Option<Target>, LabelResolverError> {
        if let Some(config) = self.toolchain_configs.get(&label.name()) {
            let target = Target::new(label.clone(), label.url().as_ref(), config.clone());
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
