use super::*;
use async_compression::futures::bufread::GzipDecoder;
use futures::StreamExt;
use futures::TryStreamExt;
use fxhash::*;
use std::path::PathBuf;
use thiserror::*;
use tokio::fs;
use tokio::io::AsyncWriteExt;
use tokio_util::compat::TokioAsyncReadCompatExt;
use tracing::*;

#[derive(Error, Debug)]
pub enum RemoteWorkspaceResolverError {
    #[error("Can't resolve a workspace with a URL that has no host: {0:?}")]
    UrlHadNoHost(url::Url),

    #[error(r#"Remote workspace for host {0} needs to be configured in the Workspace.toml - you can do that by adding this:

[remote_workspaces]
{0} = {{ url = "https://{0}/path/to/archive.tar.gz", sha1 = ".." }}

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

    #[error("Could not stream download of remote workspace {} at {path:?}, due to: {err:?} - config: {config:?}", config.url().to_string())]
    StreamingError {
        path: PathBuf,
        err: std::io::Error,
        config: RemoteWorkspaceConfig,
    },

    #[error("Could not download remote workspace {config:?}, due to: {err:?}")]
    CouldNotDownload {
        config: RemoteWorkspaceConfig,
        err: reqwest::Error,
    },

    #[error(
        "Could not extract remote workspace {config:?} from {src:?} to {src:?}, due to: {err:?}"
    )]
    ExtractionError {
        config: RemoteWorkspaceConfig,
        src: PathBuf,
        dst: PathBuf,
        err: anyhow::Error,
    },
}

#[derive(Debug)]
pub struct RemoteWorkspaceResolver {
    remote_workspace_configs: FxHashMap<String, RemoteWorkspaceConfig>,
    global_workspaces_path: PathBuf,
    client: reqwest::Client,
}

impl RemoteWorkspaceResolver {
    #[tracing::instrument(name = "RemoteWorkspaceResolver::new", skip(workspace))]
    pub fn new(workspace: &Workspace) -> Self {
        Self {
            remote_workspace_configs: workspace.remote_workspace_configs.clone(),
            global_workspaces_path: workspace.paths.global_workspaces_path.clone(),
            client: reqwest::Client::new(),
        }
    }

    #[tracing::instrument(name = "RemoteWorkspaceResolver::get", skip(self))]
    pub async fn get(&self, label: &Label) -> Result<Option<Target>, RemoteWorkspaceResolverError> {
        let host = label
            .url()
            .host()
            .ok_or_else(|| RemoteWorkspaceResolverError::UrlHadNoHost(label.url().clone()))?
            .to_string();

        if let Some(config) = self.remote_workspace_configs.get(&host) {
            self.ensure_workspace(config).await?;
            // NOTE(@ostera): once we know that we have a workspace ready in this
            // folder, we can use the current label and _reparent it_ to use the
            // path to this workspace.
            let label_path = format!(".{}", label.url().path());
            let workspace_path = self._store_path(&config);

            // here we know where we are exactly

            panic!("oh god no: {:?} ", workspace_path.join(label_path));
            return Ok(None);
        }

        Err(RemoteWorkspaceResolverError::MissingConfig(
            host.to_string(),
        ))
    }

    fn _extract_path(&self, config: &RemoteWorkspaceConfig) -> PathBuf {
        self._archive_path(config).parent().unwrap().to_path_buf()
    }

    fn _store_path(&self, config: &RemoteWorkspaceConfig) -> PathBuf {
        self._extract_path(config).join(config.prefix())
    }

    fn _warp_lock_path(&self, config: &RemoteWorkspaceConfig) -> PathBuf {
        self._store_path(config).join("Warp.lock")
    }

    fn _archive_path(&self, config: &RemoteWorkspaceConfig) -> PathBuf {
        self.global_workspaces_path
            .join(config.url().to_string().replace(':', "").replace("//", "/"))
    }

    #[tracing::instrument(name = "RemoteWorkspaceResolver::ensure_workspace", skip(self))]
    async fn ensure_workspace(
        &self,
        config: &RemoteWorkspaceConfig,
    ) -> Result<(), RemoteWorkspaceResolverError> {
        // NOTE(@ostera): if the lock file is there, we assume its already downloaded
        if self.lock_exists(config).await {
            return Ok(());
        }
        self.download(config).await?;
        self.extract(config).await?;
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

    #[tracing::instrument(name = "RemoteWorkspaceResolver::download", skip(self))]
    async fn download(
        &self,
        config: &RemoteWorkspaceConfig,
    ) -> Result<Option<PathBuf>, RemoteWorkspaceResolverError> {
        let response = self
            .client
            .get(config.url().clone())
            .send()
            .await
            .map_err(|err| RemoteWorkspaceResolverError::CouldNotDownload {
                config: config.clone(),
                err,
            })?;

        if response.status().is_success() {
            let path = self._archive_path(config);
            self.stream_response(response, path.clone())
                .await
                .map_err(|err| RemoteWorkspaceResolverError::StreamingError {
                    config: config.clone(),
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

    async fn extract(
        &self,
        config: &RemoteWorkspaceConfig,
    ) -> Result<(), RemoteWorkspaceResolverError> {
        let src = self._archive_path(config);
        let dst = self._extract_path(config);
        self.run_extraction(&src, &dst).await.map_err(|err| {
            RemoteWorkspaceResolverError::ExtractionError {
                config: config.clone(),
                src,
                dst,
                err,
            }
        })
    }

    async fn run_extraction(&self, archive: &PathBuf, dst: &PathBuf) -> Result<(), anyhow::Error> {
        let mut file = fs::File::open(&archive).await?;
        match async_zip::read::seek::ZipFileReader::new(&mut file).await {
            Ok(mut zip) => {
                for i in 0..zip.entries().len() {
                    let reader = zip.entry_reader(i).await?;

                    if reader.entry().dir() {
                        continue;
                    }

                    let file_path = PathBuf::from(reader.entry().name());
                    let path = dst.join(&file_path);
                    fs::create_dir_all(path.parent().unwrap()).await?;

                    let mut output = fs::File::create(path).await?;
                    reader.copy_to_end_crc(&mut output, 65536).await?;
                }
            }
            Err(_) => {
                let file = fs::File::open(&archive).await?;
                let decompress_stream =
                    GzipDecoder::new(futures::io::BufReader::new(file.compat()));
                let tar = async_tar::Archive::new(decompress_stream);
                tar.unpack(dst).await?;
            }
        }

        fs::remove_file(&archive).await?;

        Ok(())
    }
}
