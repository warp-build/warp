use super::*;
use async_compression::futures::bufread::GzipDecoder;
use futures::StreamExt;
use futures::TryStreamExt;
use sha2::{Digest, Sha256};
use std::path::PathBuf;
use thiserror::*;
use tokio::fs;
use tokio::io::AsyncWriteExt;
use tokio_util::compat::TokioAsyncReadCompatExt;
use url::Url;

#[derive(Clone, Debug)]
pub struct Archive {
    pub url: Url,
    pub path: PathBuf,
    pub hash: String,
}

#[derive(Debug)]
pub struct ArchiveManager {
    client: reqwest::Client,
    global_archives_root: PathBuf,
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

    #[error("Could not stream download of {url:?} due to: {err:?}")]
    StreamingError { url: Url, err: std::io::Error },

    #[error(
        "Could not extract archive originating at {url:?} from {src:?} to {src:?}, due to: {err:?}"
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
    pub fn new(workspace: &Workspace) -> Self {
        Self {
            client: reqwest::Client::new(),
            global_archives_root: workspace.paths.global_archives_root.clone(),
        }
    }

    fn _archive_path(&self, url: &Url, hash: &str) -> PathBuf {
        let scheme_and_host = PathBuf::from(url.scheme()).join(url.host_str().unwrap());
        self.global_archives_root
            .join(scheme_and_host)
            .join(hash)
            .with_extension("zip")
    }

    #[tracing::instrument(name = "ArchiveManager::download_and_extract", skip(self))]
    pub async fn download_and_extract(
        &self,
        url: &Url,
        prefix: &PathBuf,
        expected_hash: Option<String>,
    ) -> Result<Option<Archive>, ArchiveManagerError> {
        let response = self.client.get(url.clone()).send().await.map_err(|err| {
            ArchiveManagerError::CouldNotDownload {
                url: url.clone(),
                err,
            }
        })?;

        if response.status().is_success() {
            let (downloaded_file, actual_hash) = self
                .stream_response(url, response)
                .await
                .map_err(|err| ArchiveManagerError::StreamingError {
                    url: url.clone(),
                    err,
                })?;

            let scheme_and_host = PathBuf::from(url.scheme()).join(url.host_str().unwrap());

            // FIXME(@ostera): should we really trust github.com here and skip the hash check?
            let final_dir = if url.host_str().unwrap().contains("github.com") {
                prefix.join(scheme_and_host).join(expected_hash.unwrap())
            } else {
                self.check_hash(url, expected_hash, &actual_hash)?;
                prefix.join(scheme_and_host).join(&actual_hash)
            };

            self.extract(url, &downloaded_file, &final_dir).await?;

            Ok(Some(Archive {
                url: url.clone(),
                path: final_dir.clone(),
                hash: actual_hash.clone(),
            }))
        } else {
            Ok(None)
        }
    }

    fn check_hash(
        &self,
        url: &Url,
        expected_hash: Option<String>,
        actual_hash: &str,
    ) -> Result<(), ArchiveManagerError> {
        if let Some(expected) = expected_hash {
            if expected != actual_hash {
                return Err(ArchiveManagerError::HashMismatch {
                    url: url.clone(),
                    expected,
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

        let path = self._archive_path(url, &hash);
        fs::create_dir_all(&path.parent().unwrap()).await?;
        tempfile.persist(&path)?;

        Ok((path, hash))
    }

    async fn extract(
        &self,
        url: &Url,
        src: &PathBuf,
        dst: &PathBuf,
    ) -> Result<(), ArchiveManagerError> {
        self.run_extraction(src, dst)
            .await
            .map_err(|err| ArchiveManagerError::ExtractionError {
                url: url.clone(),
                src: src.to_path_buf(),
                dst: dst.to_path_buf(),
                err,
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
        Ok(())
    }
}
