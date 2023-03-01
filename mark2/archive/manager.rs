use super::{Archive, ArchiveBuilderError};
use crate::Config;
use futures::{StreamExt, TryStreamExt};
use sha2::{Digest, Sha256};
use std::path::PathBuf;
use thiserror::Error;
use tokio::{fs, io::AsyncWriteExt};
use url::Url;

#[derive(Builder)]
pub struct ArchiveManager {
    client: reqwest::Client,
    archives_root: PathBuf,
}

impl ArchiveManager {
    pub fn new(config: &Config) -> Self {
        Self {
            client: reqwest::Client::new(),
            archives_root: config.warp_root().join("archives"),
        }
    }

    pub async fn download(&self, url: &Url) -> Result<Archive, ArchiveManagerError> {
        let response = self.client.get(url.clone()).send().await?;

        if response.status().is_success() {
            let (final_path, hash) = self.stream_response(response, &url).await?;

            let archive = Archive::builder()
                .final_path(final_path)
                .hash(hash)
                .build()?;

            return Ok(archive);
        }

        Err(ArchiveManagerError::DownloadFailed {
            url: url.clone(),
            err: response.status(),
        })
    }

    async fn stream_response(
        &self,
        response: reqwest::Response,
        url: &Url,
    ) -> Result<(PathBuf, String), std::io::Error> {
        let mut byte_stream = response
            .bytes_stream()
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e));

        let tmp_root = self.archives_root.join("_tmp");
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

        let path = self.archive_path(url, &hash);
        fs::create_dir_all(&path.parent().unwrap()).await?;
        tempfile.persist(&path)?;

        Ok((path, hash))
    }

    fn archive_path(&self, url: &Url, hash: &str) -> PathBuf {
        let scheme_and_host = PathBuf::from(url.scheme()).join(url.host_str().unwrap());
        self.archives_root.join(scheme_and_host).join(hash)
    }
}

#[derive(Error, Debug)]
pub enum ArchiveManagerError {
    #[error("Could not download URL {url:?} due to: {err:?}")]
    DownloadFailed { url: Url, err: hyper::StatusCode },

    #[error(transparent)]
    RequestError(reqwest::Error),

    #[error(transparent)]
    IoError(std::io::Error),

    #[error(transparent)]
    ArchiveBuilderError(ArchiveBuilderError),
}

impl From<std::io::Error> for ArchiveManagerError {
    fn from(value: std::io::Error) -> Self {
        ArchiveManagerError::IoError(value)
    }
}

impl From<reqwest::Error> for ArchiveManagerError {
    fn from(value: reqwest::Error) -> Self {
        ArchiveManagerError::RequestError(value)
    }
}

impl From<ArchiveBuilderError> for ArchiveManagerError {
    fn from(value: ArchiveBuilderError) -> Self {
        ArchiveManagerError::ArchiveBuilderError(value)
    }
}
