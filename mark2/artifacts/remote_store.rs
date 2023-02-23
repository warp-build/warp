use super::Event;
use super::*;
use async_compression::tokio::write::GzipEncoder;
use futures::stream::TryStreamExt;
use std::path::PathBuf;
use crate::sync::Arc;
use tokio::io::AsyncReadExt;
use tokio::io::AsyncWriteExt;
use tracing::*;

/// The RemoteArtifactStore implements an external cache.
///
#[derive(Debug, Clone)]
pub struct RemoteArtifactStore {
    api: API,
    client: reqwest::Client,
    event_channel: Arc<EventChannel>,
}

impl RemoteArtifactStore {
    #[tracing::instrument(name = "RemoteArtifactStore::new", skip(workspace))]
    pub fn new(workspace: &Workspace, event_channel: Arc<EventChannel>) -> Self {
        RemoteArtifactStore {
            api: API::from_workspace(workspace),
            client: reqwest::Client::builder().gzip(false).build().unwrap(),
            event_channel,
        }
    }

    #[tracing::instrument(name = "RemoteArtifactStore::save", skip(artifacts))]
    pub async fn save(
        &self,
        key: &ArtifactStoreKey,
        artifacts: &[PathBuf],
        sandbox_root: &PathBuf,
    ) -> Result<(), ArtifactStoreError> {
        // build the archive first
        let archive = {
            let mut b = async_tar::Builder::new(vec![]);
            for artifact in artifacts {
                let path = sandbox_root.join(artifact);
                b.append_path_with_name(&path, &artifact)
                    .await
                    .map_err(|err| ArtifactStoreError::CouldNotAddArtifactToArchive {
                        err,
                        path,
                        artifact: artifact.clone(),
                    })?;
            }
            b.finish().await.unwrap();
            b.into_inner().await.unwrap()
        };

        // then compress it
        let body = {
            let mut encoder = GzipEncoder::new(vec![]);
            encoder.write_all(&archive).await.unwrap();
            encoder.shutdown().await.unwrap();
            encoder.into_inner()
        };

        self.api
            .upload_artifact(key, &body)
            .await
            .map_err(ArtifactStoreError::ApiError)?;

        Ok(())
    }

    #[tracing::instrument(name = "RemoteArtifactStore::try_fetch")]
    pub async fn try_fetch(
        &self,
        key: &ArtifactStoreKey,
        dst: &PathBuf,
        label: &LocalLabel,
    ) -> Result<(), ArtifactStoreError> {
        let url = format!("{}/artifact/{}.tar.gz", self.api.url, key);

        let response = self
            .client
            .get(&url)
            .send()
            .await
            .map_err(ArtifactStoreError::HTTPError)?;

        if response.status() == 200 {
            self.event_channel.send(Event::ArchiveDownloading {
                label: label.to_owned().into(),
                url: url.to_string(),
            });

            let byte_stream = response
                .bytes_stream()
                .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e));
            let byte_reader = tokio_util::io::StreamReader::new(byte_stream);
            let mut unzip_stream = async_compression::tokio::bufread::GzipDecoder::new(byte_reader);

            let mut data = vec![];
            unzip_stream.read_to_end(&mut data).await.unwrap();

            let dst = dst.clone();
            tokio::task::spawn_blocking(move || {
                let mut tar = tar::Archive::new(std::io::BufReader::new(&*data));
                tar.unpack(dst).unwrap();
                Ok(())
            })
            .await
            .unwrap()?;
        }

        Ok(())
    }
}
