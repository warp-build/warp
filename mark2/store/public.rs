use super::*;
use crate::Config;
use futures::stream::TryStreamExt;
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

    pub async fn try_fetch(&self, id: &ArtifactId) -> Result<(), PublicStoreError> {
        let mut url = self.config.public_store_cdn_url().clone();
        url.set_path(&format!("{}.tar.gz", id.inner()));

        let response = self.client.get(url).send().await?;

        if response.status() == 200 {
            let byte_stream = response
                .bytes_stream()
                .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e));
            let byte_reader = tokio_util::io::StreamReader::new(byte_stream);
            let mut unzip_stream = async_compression::tokio::bufread::GzipDecoder::new(byte_reader);

            let mut data = vec![];
            unzip_stream.read_to_end(&mut data).await?;

            let dst = self.config.store_root().join(id.clone());
            tokio::task::spawn_blocking(move || {
                let mut tar = tar::Archive::new(std::io::BufReader::new(&*data));
                tar.unpack(dst)
            })
            .await
            .unwrap()?;

            return Ok(());
        }

        Err(PublicStoreError::CouldNotDownloadArtifact { id: id.clone() })
    }
}

#[derive(Error, Debug)]
pub enum PublicStoreError {
    #[error("Could not download public artifact {id:?}")]
    CouldNotDownloadArtifact { id: ArtifactId },

    #[error(transparent)]
    RequestError(reqwest::Error),

    #[error(transparent)]
    IoError(std::io::Error),
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

        let config = Config::builder()
            .store_root(store_root.path().to_path_buf())
            .public_store_cdn_url(mockito::server_url().parse().unwrap())
            .build()
            .unwrap();
        let ps = PublicStore::new(config);

        let _m = mockito::mock("GET", "/sample_artifact.tar.gz")
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
        let config = Config::builder()
            .public_store_cdn_url(mockito::server_url().parse().unwrap())
            .build()
            .unwrap();
        let ps = PublicStore::new(config);

        let _m = mockito::mock("GET", "/sample_artifact.tar.gz")
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
        let config = Config::builder()
            .public_store_cdn_url(mockito::server_url().parse().unwrap())
            .build()
            .unwrap();
        let ps = PublicStore::new(config);

        let _m = mockito::mock("GET", "/sample_artifact.tar.gz")
            .with_status(200)
            .with_body("")
            .create();

        assert!(ps
            .try_fetch(&ArtifactId::new("sample_artifact"))
            .await
            .is_err());
    }
}
