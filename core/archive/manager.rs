use super::{Archive, ArchiveBuilderError};
use crate::Config;
use async_compression::futures::bufread::GzipDecoder;
use futures::{StreamExt, TryStreamExt};
use sha2::{Digest, Sha256};
use std::io::Read;
use std::path::{Path, PathBuf};
use thiserror::Error;
use tokio::fs;
use tokio::io::{AsyncReadExt, AsyncWriteExt};
use tokio_util::compat::TokioAsyncReadCompatExt;
use tracing::instrument;
use url::Url;

#[derive(Debug, Builder)]
pub struct ArchiveManager {
    client: reqwest::Client,
    archive_root: PathBuf,
}

impl ArchiveManager {
    pub fn new(config: &Config) -> Self {
        Self {
            client: config.http_client().clone(),
            archive_root: config.archive_root().to_path_buf(),
        }
    }

    #[instrument(name = "ArchiveManager::find", skip(self))]
    pub async fn find(&self, url: &Url) -> Result<Option<Archive>, ArchiveManagerError> {
        let path = self.archive_path(url);

        if fs::File::open(&path).await.is_err() {
            return Ok(None);
        };

        let mut hash_file = fs::File::open(path.with_extension("sha256")).await.unwrap();
        let mut hash = String::new();
        hash_file.read_to_string(&mut hash).await.unwrap();

        Ok(Some(
            Archive::builder()
                .final_path(path)
                .hash(hash)
                .url(url.clone())
                .build()
                .unwrap(),
        ))
    }

    #[instrument(name = "ArchiveManager::extract", skip(self))]
    pub async fn extract(
        &self,
        archive: &Archive,
        root: &Path,
    ) -> Result<Archive, ArchiveManagerError> {
        let file = fs::File::open(archive.final_path()).await?;
        {
            let root = root.to_path_buf();
            let mut data = vec![];
            let mut unzip_stream = GzipDecoder::new(futures::io::BufReader::new(file.compat()));

            if futures::AsyncReadExt::read_to_end(&mut unzip_stream, &mut data)
                .await
                .is_err()
            {
                data = vec![];
                let mut file = std::fs::File::open(archive.final_path()).unwrap();
                file.read_to_end(&mut data)?;
            };

            tokio::task::spawn_blocking(move || {
                let mut tar = tar::Archive::new(std::io::BufReader::new(&*data));
                tar.unpack(root)
            })
            .await
            .unwrap()
        }?;

        let archive = Archive::builder()
            .final_path(root)
            .hash(archive.hash())
            .url(archive.url().clone())
            .build()?;

        Ok(archive)
    }

    #[instrument(name = "ArchiveManager::download", skip(self))]
    pub async fn download(&self, url: &Url) -> Result<Archive, ArchiveManagerError> {
        if let Some(archive) = self.find(&url).await? {
            return Ok(archive);
        }

        let response = self.client.get(url.clone()).send().await?;

        if response.status().is_success() {
            let (final_path, hash) = self.stream_response(response, url).await?;

            let archive = Archive::builder()
                .final_path(final_path)
                .hash(hash)
                .url(url.clone())
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

        let tmp_root = self.archive_root.join("_tmp");
        fs::create_dir_all(&tmp_root).await?;

        let tempfile = tempfile::NamedTempFile::new_in(&tmp_root)?;
        let mut outfile = fs::File::from_std(tempfile.reopen()?);

        let mut s = Sha256::new();
        while let Some(chunk) = byte_stream.next().await {
            let mut chunk = chunk?;
            s.update(&chunk);
            outfile.write_all_buf(&mut chunk).await?;
        }

        let path = self.archive_path(url);
        fs::create_dir_all(&path.parent().unwrap()).await?;
        tempfile.persist(&path)?;

        let hash = format!("{:x}", s.finalize());
        fs::write(path.with_extension("sha256"), &hash).await?;

        Ok((path, hash))
    }

    fn archive_path(&self, url: &Url) -> PathBuf {
        let mut s = Sha256::new();
        s.update(&url.to_string());
        let hash = format!("{:x}", s.finalize());

        let scheme_and_host = PathBuf::from(url.scheme()).join(url.host_str().unwrap());
        self.archive_root.join(scheme_and_host).join(hash)
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

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn downloads_file_to_archives_folder() {
        let archive_root = assert_fs::TempDir::new().unwrap();

        let config = Config::builder()
            .archive_root(archive_root.path().to_path_buf())
            .build()
            .unwrap();
        let am = ArchiveManager::new(&config);

        let _m = mockito::mock("GET", "/sample_artifact.tar.gz")
            .with_status(200)
            .with_body(include_bytes!("./fixtures/sample_artifact.tar.gz"))
            .create();

        let url: Url = format!("{}/sample_artifact.tar.gz", mockito::server_url())
            .parse()
            .unwrap();
        let archive = am.download(&url).await.unwrap();
        let final_path = archive
            .final_path()
            .strip_prefix(archive_root.path())
            .unwrap();

        assert_eq!(
            final_path.to_string_lossy().to_string(),
            "http/127.0.0.1/53c7c792c4667c37741310914aa53416fe9750c8f787641e7aff55ecfdbe97d0"
        );
    }

    #[tokio::test]
    async fn fails_if_url_is_not_downloadable() {
        let config = Config::default();
        let am = ArchiveManager::new(&config);

        let _m = mockito::mock("GET", "/sample_artifact.tar.gz")
            .with_status(400)
            .create();

        let url: Url = format!("{}/sample_artifact.tar.gz", mockito::server_url())
            .parse()
            .unwrap();
        assert!(am.download(&url).await.is_err());
    }
}
