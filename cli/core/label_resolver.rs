use super::*;
use futures::stream::TryStreamExt;
use futures::StreamExt;
use std::fs::Metadata;
use std::path::PathBuf;
use thiserror::*;
use tokio::fs;
use tokio::io::AsyncWriteExt;
use tracing::*;

#[derive(Debug)]
pub struct LabelResolver {
    global_rules_root: PathBuf,
    client: reqwest::Client,
    workspace_root: PathBuf,
}

#[derive(Error, Debug)]
pub enum LabelResolverError {
    #[error(transparent)]
    BuildfileError(BuildfileError),

    #[error("Could not read label {label:?} at {path:?}, due to: {err:?}")]
    FileSystemError {
        label: Label,
        path: PathBuf,
        err: std::io::Error,
    },

    #[error("Expected label {label:?} at {path:?} to be a file, but instead found this metadata: {meta:#?}")]
    LabelPathIsNotAFile {
        label: Label,
        path: PathBuf,
        meta: Metadata,
    },

    #[error("Could not resolve label: {0:?}")]
    CouldNotResolveLabel(Label),

    #[error("Could not find target: {0:?}")]
    TargetNotFound(Label),

    #[error("Could not download label {label:?} from {:?}, due to: {err:?}", label.url())]
    CouldNotDownload { label: Label, err: reqwest::Error },
}

impl LabelResolver {
    pub fn new(workspace: &Workspace) -> Self {
        Self {
            global_rules_root: workspace.paths.global_rules_root.clone(),
            workspace_root: workspace.paths.workspace_root.clone(),
            client: reqwest::Client::new(),
        }
    }

    pub async fn resolve(&self, label: &Label) -> Result<Target, LabelResolverError> {
        let path = if label.is_remote() {
            self.fetch(label).await
        } else {
            Ok(Some(self.workspace_root.join(label.path())))
        }?
        .ok_or_else(|| LabelResolverError::CouldNotResolveLabel(label.clone()))?
        .join(BUILDFILE);

        let buildfile = Buildfile::from_file(&path)
            .await
            .map_err(LabelResolverError::BuildfileError)?;

        buildfile
            .targets
            .iter()
            .find(|t| t.label.name() == *label.name())
            .ok_or_else(|| LabelResolverError::TargetNotFound(label.clone()))
            .cloned()
    }

    async fn fetch(&self, label: &Label) -> Result<Option<PathBuf>, LabelResolverError> {
        if let Some(path) = self.find_in_disk(label).await? {
            return Ok(Some(path));
        }
        self.download(label).await
    }

    async fn find_in_disk(&self, label: &Label) -> Result<Option<PathBuf>, LabelResolverError> {
        if label.is_remote() {
            let path = self.global_rules_root.join(label.as_store_prefix());
            let meta =
                fs::metadata(&path)
                    .await
                    .map_err(|err| LabelResolverError::FileSystemError {
                        label: label.clone(),
                        path: path.clone(),
                        err,
                    });
            if meta.is_err() {
                Ok(None)
            } else {
                Ok(Some(path))
            }
        } else {
            Ok(Some(label.path()))
        }
    }

    async fn download(&self, label: &Label) -> Result<Option<PathBuf>, LabelResolverError> {
        let response = self.client.get(label.url()).send().await.map_err(|err| {
            LabelResolverError::CouldNotDownload {
                label: label.clone(),
                err,
            }
        })?;

        if response.status().is_success() {
            let path = self.global_rules_root.join(label.as_store_prefix());

            self.stream_response(response, path.clone())
                .await
                .map_err(|err| LabelResolverError::FileSystemError {
                    label: label.clone(),
                    path: path.clone(),
                    err,
                })?;

            Ok(Some(path))
        } else {
            Ok(None)
        }
    }

    async fn stream_response(
        &self,
        response: reqwest::Response,
        path: PathBuf,
    ) -> Result<(), std::io::Error> {
        let mut byte_stream = response
            .bytes_stream()
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e));

        fs::create_dir_all(&path.parent().unwrap()).await?;
        let mut outfile = fs::File::create(&path).await?;
        while let Some(chunk) = byte_stream.next().await {
            outfile.write_all_buf(&mut chunk?).await?;
        }
        Ok(())
    }
}

mod tests {
    use super::*;

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
