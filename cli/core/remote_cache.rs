use super::*;
use async_compression::futures::bufread::GzipDecoder;
use async_compression::tokio::write::GzipEncoder;
use futures::stream::TryStreamExt;
use futures::StreamExt;
use reqwest::header::*;
use std::path::PathBuf;
use tokio::fs;
use tokio::io::AsyncWriteExt;
use tokio_util::compat::TokioAsyncReadCompatExt;
use tracing::*;
use url::Url;

/// The RemoteCache implements an external cache.
///
#[derive(Debug, Clone)]
pub struct RemoteCache {
    global_root: PathBuf,
    local_root: PathBuf,
    url: Url,
}

impl RemoteCache {
    #[tracing::instrument(name = "RemoteCache::new", skip(workspace))]
    pub fn new(workspace: &Workspace) -> RemoteCache {
        RemoteCache {
            global_root: workspace.paths.global_cache_root.clone(),
            local_root: workspace.paths.local_cache_root.clone(),
            url: workspace.remote_cache_url.clone(),
        }
    }

    #[tracing::instrument(name = "RemoteCache::save", skip(sandbox))]
    pub async fn save(&mut self, sandbox: &LocalSandbox) -> Result<(), anyhow::Error> {
        let node = sandbox.node();
        let hash = node.hash();

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

        let client = reqwest::Client::builder().gzip(false).build()?;
        let url = format!("{}/artifact/{}.tar.gz", self.url, &hash);
        let response = client.post(url).send().await?;
        let upload_url: std::collections::HashMap<String, String> = response.json().await?;
        let upload_url: url::Url = upload_url.get("signed_url").unwrap().parse()?;

        let mut headers = reqwest::header::HeaderMap::new();
        headers.insert("ACL", "public-read".parse()?);
        headers.insert(CONTENT_TYPE, "application/octet-stream".parse()?);

        let request = client
            .put(upload_url)
            // .query(&[("x-amz-acl", "public-read")])
            .headers(headers)
            .body(body)
            .build()?;

        let response = client.execute(request).await?;

        if response.status().as_u16() >= 400 {
            println!("{}", response.text().await?);
        }

        Ok(())
    }

    #[tracing::instrument(name = "RemoteCache::try_fetch", skip(node))]
    pub async fn try_fetch(&mut self, node: &ComputedTarget) -> Result<(), anyhow::Error> {
        let hash = node.hash();

        let dst = if node.target.is_pinned() {
            self.global_root.join(&hash)
        } else {
            self.local_root.join(&hash)
        };

        let dst_tarball = &dst.with_extension("tar.gz");

        let url = format!("{}/artifact/{}.tar.gz", self.url, hash);
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
