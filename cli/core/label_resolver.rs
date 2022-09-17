use super::*;
use fxhash::*;
use std::path::PathBuf;
use thiserror::*;
use tracing::*;

#[derive(Debug)]
pub struct LabelResolver {
    workspace_root: PathBuf,
    toolchain_configs: FxHashMap<String, RuleConfig>,
}

#[derive(Error, Debug)]
pub enum LabelResolverError {
    #[error(transparent)]
    BuildfileError(BuildfileError),

    #[error("Remote label {0:?} needs to be configured in the Workspace.toml")]
    RemoteLabelNeedsConfig(Label),

    #[error("Could not find target: {0:?}")]
    TargetNotFound(Label),
}

impl LabelResolver {
    pub fn new(workspace: &Workspace) -> Self {
        Self {
            toolchain_configs: workspace.toolchain_configs.clone(),
            workspace_root: workspace.paths.workspace_root.clone(),
        }
    }

    #[tracing::instrument(name = "LabelResolver::resolve", skip(self))]
    pub async fn resolve(&self, label: &Label) -> Result<Target, LabelResolverError> {
        if label.is_remote() {
            return self.resolve_remote(label);
        }

        let buildfile = Buildfile::from_label(&self.workspace_root, label)
            .await
            .map_err(LabelResolverError::BuildfileError)?;

        buildfile
            .targets
            .iter()
            .find(|t| t.label.name() == *label.name())
            .ok_or_else(|| LabelResolverError::TargetNotFound(label.clone()))
            .cloned()
    }

    #[tracing::instrument(name = "LabelResolver::resolve_remote", skip(self))]
    pub fn resolve_remote(&self, label: &Label) -> Result<Target, LabelResolverError> {
        if let Some(config) = self.toolchain_configs.get(&label.name()) {
            let target = Target::new(label.clone(), label.url().as_ref(), config.clone());
            Ok(target)
        } else {
            Err(LabelResolverError::RemoteLabelNeedsConfig(label.clone()))
        }
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
        file.write(buildfile.as_bytes()).await.unwrap();
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
                WorkspaceConfig::builder()
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

        dbg!(&workspace);

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
