use super::*;
use crate::events::event::ArchiveEvent;
use crate::Config;
use async_compression::tokio::bufread::GzipDecoder;
use futures::stream::TryStreamExt;
use sha2::{Digest, Sha256};
use thiserror::*;
use tokio::io::AsyncReadExt;
use tracing::*;

/// The PublicStore implements access to the public cache serviced by Warp.
///
#[derive(Debug, Clone)]
pub struct PublicStore {
    config: Config,
    client: reqwest::Client,
}

impl PublicStore {
    pub fn new(config: Config) -> Self {
        Self {
            client: config.http_client().clone(),
            config,
        }
    }

    pub async fn try_fetch(
        &self,
        id: &ArtifactId,
    ) -> Result<PublicArtifactDownload, PublicStoreError> {
        let ec = self.config.event_channel();

        if self.config.offline() {
            return Err(PublicStoreError::Offline);
        }

        let mut url = self.config.public_store_cdn_url().clone();
        url.set_path(&format!("{}.tar.gz", id.inner()));

        ec.send(ArchiveEvent::DownloadStarted { url: url.clone() });

        let response = self.client.get(url.clone()).send().await?;

        if response.status() == 200 {
            let byte_stream = response
                .bytes_stream()
                .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e));
            let byte_reader = tokio_util::io::StreamReader::new(byte_stream);
            let mut unzip_stream = GzipDecoder::new(byte_reader);

            let mut data = vec![];
            unzip_stream.read_to_end(&mut data).await?;

            let mut s = Sha256::new();
            s.update(&data);
            let sha256 = format!("{:x}", s.finalize());
            let total_size = data.len() as u64;

            let dst = self.config.artifact_store_root().join(id.clone());
            tokio::task::spawn_blocking(move || {
                let mut tar = tar::Archive::new(std::io::BufReader::new(&*data));
                tar.unpack(dst)
            })
            .await
            .unwrap()?;

            ec.send(ArchiveEvent::DownloadCompleted {
                url,
                sha256,
                total_size,
            });

            return Ok(PublicArtifactDownload::Downloaded);
        }

        Ok(PublicArtifactDownload::Missing)
    }
}

pub enum PublicArtifactDownload {
    Downloaded,
    Missing,
}

#[derive(Error, Debug)]
pub enum PublicStoreError {
    #[error("Could not download public artifact {id:?}")]
    CouldNotDownloadArtifact { id: ArtifactId },

    #[error(transparent)]
    RequestError(reqwest::Error),

    #[error(transparent)]
    IoError(std::io::Error),

    #[error("Can't download from the public store in offline mode.")]
    Offline,
}

impl From<reqwest::Error> for PublicStoreError {
    fn from(value: reqwest::Error) -> Self {
        PublicStoreError::RequestError(value)
    }
}

impl From<std::io::Error> for PublicStoreError {
    fn from(value: std::io::Error) -> Self {
        PublicStoreError::IoError(value)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use assert_fs::prelude::*;

    #[tokio::test]
    async fn downloads_and_extracts_artifact() {
        let store_root = assert_fs::TempDir::new().unwrap();

        let mut server = mockito::Server::new_async().await;
        let mock_url = server.url().parse::<Url>().unwrap();

        let config = Config::builder()
            .artifact_store_root(store_root.path().to_path_buf())
            .public_store_cdn_url(mock_url.clone())
            .build()
            .unwrap();
        let ps = PublicStore::new(config);

        let _m = server
            .mock("GET", "/sample_artifact.tar.gz")
            .with_status(200)
            .with_body(include_bytes!("./fixtures/sample_artifact.tar.gz"))
            .create();

        assert!(ps
            .try_fetch(&ArtifactId::new("sample_artifact"))
            .await
            .is_ok());

        let actual_file = store_root.child("sample_artifact/Manifest.json");

        assert!(actual_file.exists());

        let expected_file = include_str!("./fixtures/Manifest.json");
        actual_file.assert(expected_file);
    }

    #[tokio::test]
    async fn fails_on_non_targz_payload() {
        let mut server = mockito::Server::new_async().await;
        let mock_url = server.url().parse::<Url>().unwrap();

        let config = Config::builder()
            .public_store_cdn_url(mock_url)
            .build()
            .unwrap();
        let ps = PublicStore::new(config);

        let _m = server
            .mock("GET", "/sample_artifact.tar.gz")
            .with_status(200)
            .with_body("garbage data")
            .create();

        assert!(ps
            .try_fetch(&ArtifactId::new("sample_artifact"))
            .await
            .is_err());
    }

    #[tokio::test]
    async fn fails_on_empty_payload() {
        let mut server = mockito::Server::new_async().await;
        let mock_url = server.url().parse::<Url>().unwrap();

        let config = Config::builder()
            .public_store_cdn_url(mock_url)
            .build()
            .unwrap();
        let ps = PublicStore::new(config);

        let _m = server
            .mock("GET", "/sample_artifact.tar.gz")
            .with_status(200)
            .with_body("")
            .create();

        assert!(ps
            .try_fetch(&ArtifactId::new("sample_artifact"))
            .await
            .is_err());
    }
}
