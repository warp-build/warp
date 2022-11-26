use super::Event;
use super::*;
use async_compression::futures::bufread::GzipDecoder;
use futures::AsyncReadExt;
use futures::StreamExt;
use futures::TryStreamExt;
use sha2::{Digest, Sha256};
use std::path::Path;
use std::path::PathBuf;
use std::sync::Arc;
use thiserror::*;
use tokio::fs;
use tokio::io::AsyncWriteExt;
use tokio_util::compat::TokioAsyncReadCompatExt;
use tracing::*;
use url::Url;

#[derive(Clone, Debug)]
pub struct Archive {
    pub url: Url,
    pub path: PathBuf,
    pub hash: String,
}

#[derive(Debug, Clone)]
pub struct ArchiveManager {
    client: reqwest::Client,
    global_archives_root: PathBuf,
    event_channel: Arc<EventChannel>,
}

#[derive(Error, Debug)]
pub enum ArchiveManagerError {
    #[error(
        "File system error when working with {path:?} (originating on {url:?}), due to: {err:?}"
    )]
    FileSystemError {
        url: Url,
        path: PathBuf,
        err: std::io::Error,
    },

    #[error("Could not download {url:?} due to: {err:?}")]
    CouldNotDownload { url: Url, err: reqwest::Error },

    #[error("Failed download {url:?} with status: {err:?}")]
    DownloadFailed { url: Url, err: reqwest::StatusCode },

    #[error("Could not stream download of {url:?} due to: {err:?}")]
    StreamingError { url: Url, err: std::io::Error },

    #[error(
        "Could not copy archive originating at {url:?} from {src:?} to {dst:?}, due to: {err:?}"
    )]
    CopyError {
        url: Url,
        src: PathBuf,
        dst: PathBuf,
        err: std::io::Error,
    },

    #[error(
        "Could not extract archive originating at {} from {src:?} to {dst:?}, due to: {err:?}", url.to_string()
    )]
    ExtractionError {
        url: Url,
        src: PathBuf,
        dst: PathBuf,
        err: anyhow::Error,
    },

    #[error("We expected url '{url:?}' to have hash {expected:?} but instead we found {actual:?}")]
    HashMismatch {
        url: Url,
        expected: String,
        actual: String,
    },
}

impl ArchiveManager {
    pub fn new(workspace: &Workspace, event_channel: Arc<EventChannel>) -> Self {
        Self {
            client: reqwest::Client::new(),
            global_archives_root: workspace.paths.global_archives_root.clone(),
            event_channel,
        }
    }

    pub fn from_paths(workspace_paths: &WorkspacePaths, event_channel: Arc<EventChannel>) -> Self {
        Self {
            client: reqwest::Client::new(),
            global_archives_root: workspace_paths.global_archives_root.clone(),
            event_channel,
        }
    }

    fn _archive_path(&self, url: &Url, hash: &str, file_ext: &str) -> PathBuf {
        let scheme_and_host = PathBuf::from(url.scheme()).join(url.host_str().unwrap());
        self.global_archives_root
            .join(scheme_and_host)
            .join(hash)
            .with_extension(file_ext)
    }

    #[tracing::instrument(name = "ArchiveManager::download", skip(self))]
    pub async fn download(
        &self,
        url: &Url,
        file_ext: &str,
    ) -> Result<(PathBuf, String), ArchiveManagerError> {
        self.event_channel.send(Event::ArchiveDownloading {
            label: url.to_owned().into(),
            url: url.to_string(),
        });

        let response = self.client.get(url.clone()).send().await.map_err(|err| {
            ArchiveManagerError::CouldNotDownload {
                url: url.clone(),
                err,
            }
        })?;

        if response.status().is_success() {
            self.stream_response(url, response, file_ext)
                .await
                .map_err(|err| ArchiveManagerError::StreamingError {
                    url: url.clone(),
                    err,
                })
        } else {
            Err(ArchiveManagerError::DownloadFailed {
                url: url.clone(),
                err: response.status(),
            })
        }
    }

    #[tracing::instrument(name = "ArchiveManager::download_and_move", skip(self))]
    pub async fn download_and_move(
        &self,
        url: &Url,
        prefix: &PathBuf,
        expected_hash: Option<String>,
        file_ext: &str,
    ) -> Result<Archive, ArchiveManagerError> {
        let (downloaded_file, actual_hash) = self.download(url, file_ext).await?;

        self.check_hash(url, &expected_hash, &actual_hash)?;

        let final_dir = if expected_hash.is_some() {
            prefix.join(&actual_hash)
        } else {
            prefix.to_path_buf()
        };

        fs::copy(&downloaded_file, &final_dir)
            .await
            .map_err(|err| ArchiveManagerError::CopyError {
                url: url.clone(),
                src: downloaded_file,
                dst: final_dir.clone(),
                err,
            })?;

        Ok(Archive {
            url: url.clone(),
            path: final_dir.clone(),
            hash: actual_hash.clone(),
        })
    }

    #[tracing::instrument(name = "ArchiveManager::download_and_extract", skip(self))]
    pub async fn download_and_extract(
        &self,
        url: &Url,
        prefix: &PathBuf,
        expected_hash: Option<String>,
        strip_prefix: Option<String>,
    ) -> Result<Archive, ArchiveManagerError> {
        let (downloaded_file, actual_hash) = self.download(url, "tar.gz").await?;

        self.check_hash(url, &expected_hash, &actual_hash)?;

        let final_dir = if expected_hash.is_some() {
            prefix.join(&actual_hash)
        } else {
            prefix.to_path_buf()
        };

        self.extract(url, &downloaded_file, &final_dir, strip_prefix)
            .await?;

        Ok(Archive {
            url: url.clone(),
            path: final_dir.clone(),
            hash: actual_hash.clone(),
        })
    }

    fn check_hash(
        &self,
        url: &Url,
        expected_hash: &Option<String>,
        actual_hash: &str,
    ) -> Result<(), ArchiveManagerError> {
        if let Some(expected) = expected_hash {
            self.event_channel.send(Event::ArchiveVerifying(url.into()));
            if expected != actual_hash {
                return Err(ArchiveManagerError::HashMismatch {
                    url: url.clone(),
                    expected: expected.clone(),
                    actual: actual_hash.to_string(),
                });
            }
        }

        Ok(())
    }

    async fn stream_response(
        &self,
        url: &Url,
        response: reqwest::Response,
        file_ext: &str,
    ) -> Result<(PathBuf, String), std::io::Error> {
        let mut byte_stream = response
            .bytes_stream()
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e));

        let tmp_root = self.global_archives_root.join("_tmp");
        fs::create_dir_all(&tmp_root).await?;

        let tempfile = tempfile::NamedTempFile::new_in(&tmp_root)?;
        let mut outfile = fs::File::from_std(tempfile.reopen()?);

        let mut s = Sha256::new();
        while let Some(chunk) = byte_stream.next().await {
            let mut chunk = chunk?;
            s.update(&chunk);
            outfile.write_all_buf(&mut chunk).await?;
        }

        let hash = format!("{:x}", s.finalize());

        let path = self._archive_path(url, &hash, file_ext);
        fs::create_dir_all(&path.parent().unwrap()).await?;
        tempfile.persist(&path)?;

        Ok((path, hash))
    }

    #[tracing::instrument(name = "ArchiveManager::extract", skip(self))]
    async fn extract(
        &self,
        url: &Url,
        src: &PathBuf,
        dst: &PathBuf,
        strip_prefix: Option<String>,
    ) -> Result<(), ArchiveManagerError> {
        self.run_extraction(src, dst, strip_prefix)
            .await
            .map_err(|err| ArchiveManagerError::ExtractionError {
                url: url.clone(),
                src: src.to_path_buf(),
                dst: dst.to_path_buf(),
                err,
            })
    }

    async fn run_extraction(
        &self,
        archive: &PathBuf,
        dst: &PathBuf,
        strip_prefix: Option<String>,
    ) -> Result<(), anyhow::Error> {
        let mut file = fs::File::open(&archive).await?;
        match async_zip::read::seek::ZipFileReader::new(&mut file).await {
            Ok(mut zip) => {
                for i in 0..zip.entries().len() {
                    let reader = zip.entry_reader(i).await?;

                    if reader.entry().dir() {
                        continue;
                    }

                    let file_path = PathBuf::from(reader.entry().name());
                    let file_path = if let Some(ref prefix) = strip_prefix {
                        file_path.strip_prefix(prefix)?.to_path_buf()
                    } else {
                        file_path
                    };
                    let path = dst.join(&file_path);
                    fs::create_dir_all(path.parent().unwrap()).await?;

                    let mut output = fs::File::create(path).await?;
                    reader.copy_to_end_crc(&mut output, 65536).await?;
                }
            }
            Err(_err) => {
                if let Some(ref prefix) = strip_prefix {
                    let tmpdir = tempfile::tempdir()?;

                    self.unpack(archive, &tmpdir.path()).await?;

                    let tmp_root = tmpdir.path().join(prefix).to_path_buf();

                    let mut files = Box::pin(
                        FileScanner::new()
                            .starting_from(&tmp_root)
                            .await?
                            .stream_files()
                            .await,
                    );

                    while let Some(src_path) = files.next().await {
                        let src_path = src_path?;
                        if fs::metadata(&src_path).await?.is_dir() {
                            continue;
                        }
                        // NOTE(@ostera): when using tmpdir we create a folder that starts at `/tmp`,
                        // but our FileScanner resolves _through_ and gets the actual `/private/tmp`
                        // prefix.
                        let path = src_path
                            .strip_prefix("/private")?
                            .strip_prefix(tmp_root.strip_prefix("/").unwrap())?;

                        let dst_path = dst.join(path);
                        fs::create_dir_all(dst_path.parent().unwrap()).await?;
                        debug!("cp {:#?} -> {:#?}", &src_path, &dst_path);
                        fs::copy(src_path, dst_path).await?;
                    }
                } else {
                    self.unpack(archive, dst).await?
                }
            }
        }
        Ok(())
    }

    /// Attempt to unpack [archive] into [dst] by decompressing it first, and if it fails, try
    /// without decompression.
    async fn unpack(&self, archive: &PathBuf, dst: &Path) -> Result<(), anyhow::Error> {
        let file = fs::File::open(archive).await?;
        let mut decompress_stream = GzipDecoder::new(futures::io::BufReader::new(file.compat()));

        let mut data = vec![];
        if decompress_stream.read_to_end(&mut data).await.is_ok() {
            let mut tar = tar::Archive::new(std::io::BufReader::new(&*data));
            tar.unpack(dst)?
        } else {
            let file = std::fs::File::open(archive)?;
            let mut tar = tar::Archive::new(std::io::BufReader::new(file));
            tar.unpack(dst)?
        }
        Ok(())
    }
}
