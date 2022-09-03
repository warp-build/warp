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
    global_root: PathBuf,
    local_root: PathBuf,
}

impl RemoteCache {
    #[tracing::instrument(name = "RemoteCache::new", skip(workspace))]
    pub fn new(workspace: &Workspace) -> RemoteCache {
        RemoteCache {
            api: API::from_workspace(&workspace),
            global_root: workspace.paths.global_cache_root.clone(),
            local_root: workspace.paths.local_cache_root.clone(),
        }
    }

    #[tracing::instrument(name = "RemoteCache::save", skip(sandbox))]
    pub async fn save(&mut self, sandbox: &LocalSandbox) -> Result<(), anyhow::Error> {
        let node = sandbox.node();

        let cache_key = node.cache_key();

        let artifacts = if node.target.is_pinned() {
            sandbox.all_outputs().await
        } else {
            sandbox.outputs()
        };

        // build the archive first
        let archive = {
            let mut b = async_tar::Builder::new(vec![]);
            for artifact in &artifacts {
                b.append_path_with_name(&sandbox.root().join(&artifact), &artifact)
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

        self.api.upload_artifact(&cache_key, &body).await?;

        Ok(())
    }

    #[tracing::instrument(name = "RemoteCache::try_fetch", skip(node))]
    pub async fn try_fetch(&mut self, node: &ComputedTarget) -> Result<(), anyhow::Error> {
        let cache_key = node.cache_key();

        let dst = self.global_root.join(&cache_key);

        let dst_tarball = &dst.with_extension("tar.gz");

        let url = format!("{}/artifact/{}.tar.gz", self.api.url, cache_key);
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
