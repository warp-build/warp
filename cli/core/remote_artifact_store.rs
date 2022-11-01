use super::Event;
use super::*;
use async_compression::futures::bufread::GzipDecoder;
use async_compression::tokio::write::GzipEncoder;
use futures::stream::TryStreamExt;
use futures::StreamExt;
use std::path::PathBuf;
use std::sync::Arc;
use tokio::fs;
use tokio::io::AsyncWriteExt;
use tokio_util::compat::TokioAsyncReadCompatExt;
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
                b.append_path_with_name(&sandbox_root.join(&artifact), &artifact)
                    .await
                    .map_err(ArtifactStoreError::IOError)?;
            }
            b.finish().await.map_err(ArtifactStoreError::IOError)?;
            b.into_inner().await.map_err(ArtifactStoreError::IOError)?
        };

        // then compress it
        let body = {
            let mut encoder = GzipEncoder::new(vec![]);
            encoder
                .write_all(&archive)
                .await
                .map_err(ArtifactStoreError::IOError)?;
            encoder
                .shutdown()
                .await
                .map_err(ArtifactStoreError::IOError)?;
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
        let dst_tarball = &dst.with_extension("tar.gz");

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

            let mut byte_stream = response
                .bytes_stream()
                .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e));

            fs::create_dir_all(&dst_tarball.parent().unwrap())
                .await
                .map_err(ArtifactStoreError::IOError)?;

            let mut outfile = fs::File::create(&dst_tarball)
                .await
                .map_err(ArtifactStoreError::IOError)?;

            while let Some(chunk) = byte_stream.next().await {
                let mut chunk = chunk.map_err(ArtifactStoreError::IOError)?;

                outfile
                    .write_all_buf(&mut chunk)
                    .await
                    .map_err(ArtifactStoreError::IOError)?;
            }

            let file = fs::File::open(&dst_tarball)
                .await
                .map_err(ArtifactStoreError::IOError)?;

            let decompress_stream = GzipDecoder::new(futures::io::BufReader::new(file.compat()));

            let tar = async_tar::Archive::new(decompress_stream);
            tar.unpack(dst).await.map_err(ArtifactStoreError::IOError)?;

            fs::remove_file(&dst_tarball)
                .await
                .map_err(ArtifactStoreError::IOError)?;
        }

        Ok(())
    }
}
