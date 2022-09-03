use super::*;
use async_compression::futures::bufread::GzipDecoder;
use async_compression::tokio::write::GzipEncoder;
use futures::stream::TryStreamExt;
use futures::StreamExt;
use std::path::PathBuf;
use tokio::fs;
use tokio::io::AsyncWriteExt;
use tokio_util::compat::TokioAsyncReadCompatExt;
use tracing::*;

/// The RemoteCache implements an external cache.
///
#[derive(Debug, Clone)]
pub struct RemoteCache {
    api: API,
}

impl RemoteCache {
    #[tracing::instrument(name = "RemoteCache::new", skip(workspace))]
    pub fn new(workspace: &Workspace) -> RemoteCache {
        RemoteCache {
            api: API::from_workspace(&workspace),
        }
    }

    #[tracing::instrument(name = "RemoteCache::save", skip(artifacts))]
    pub async fn save(
        &mut self,
        key: &CacheKey,
        artifacts: &[PathBuf],
        sandbox_root: &PathBuf,
    ) -> Result<(), anyhow::Error> {
        // build the archive first
        let archive = {
            let mut b = async_tar::Builder::new(vec![]);
            for artifact in artifacts {
                b.append_path_with_name(&sandbox_root.join(&artifact), &artifact)
                    .await?;
            }
            b.finish().await?;
            b.into_inner().await?
        };

        // then compress it
        let body = {
            let mut encoder = GzipEncoder::new(vec![]);
            encoder.write_all(&archive).await?;
            encoder.shutdown().await?;
            encoder.into_inner()
        };

        self.api.upload_artifact(&key, &body).await?;

        Ok(())
    }

    #[tracing::instrument(name = "RemoteCache::try_fetch")]
    pub async fn try_fetch(&mut self, key: &CacheKey, dst: &PathBuf) -> Result<(), anyhow::Error> {
        let dst_tarball = &dst.with_extension("tar.gz");

        let url = format!("{}/artifact/{}.tar.gz", self.api.url, key);
        let client = reqwest::Client::builder().gzip(false).build()?;
        let response = client.get(url).send().await?;

        if response.status() == 200 {
            let mut byte_stream = response
                .bytes_stream()
                .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e));

            fs::create_dir_all(&dst_tarball.parent().unwrap()).await?;

            let mut outfile = fs::File::create(&dst_tarball).await?;
            while let Some(chunk) = byte_stream.next().await {
                outfile.write_all_buf(&mut chunk?).await?;
            }

            let file = fs::File::open(&dst_tarball).await?;
            let decompress_stream = GzipDecoder::new(futures::io::BufReader::new(file.compat()));
            let tar = async_tar::Archive::new(decompress_stream);
            tar.unpack(dst).await?;
            fs::remove_file(&dst_tarball).await?;
        }

        Ok(())
    }
}
