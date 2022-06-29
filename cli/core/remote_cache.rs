use super::*;
use async_compression::futures::bufread::GzipDecoder;
use futures::stream::TryStreamExt;
use reqwest::header::*;
use std::path::PathBuf;
use tokio::fs;
use tokio::io::AsyncReadExt;
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

        // build a tarball with the sandbox
        let archive_path = &sandbox.root().join(&hash).with_extension(".tar.gz");
        let archive = fs::File::create(&archive_path).await?;
        let archive = archive.compat();
        let mut b = async_tar::Builder::new(futures::io::BufWriter::new(archive));
        for artifact in &artifacts {
            b.append_path_with_name(&sandbox.root().join(&artifact), &artifact)
                .await?;
        }

        let client = reqwest::Client::new();
        let url = format!("{}/artifact/{}.tar.gz", self.url, &hash);
        let response = client.post(url).send().await?;
        let upload_url: std::collections::HashMap<String, String> = response.json().await?;

        let mut body = String::new();
        fs::File::open(archive_path)
            .await?
            .read_to_string(&mut body)
            .await?;

        let mut headers = reqwest::header::HeaderMap::new();
        // headers.insert("x-amz-acl", "public-read".parse()?);
        // headers.insert(CONTENT_LENGTH, format!("{}", body.len()).parse()?);
        // headers.insert(CONTENT_TYPE, "application/octet-stream".parse()?);

        let request = client
            .put(upload_url.get("signed_url").unwrap())
            .query(&[("x-amz-acl", "public-read")])
            .headers(headers)
            .body(body)
            .build()?;

        dbg!(&request);

        let response = client.execute(request).await?;

        if response.status().as_u16() >= 400 {
            dbg!(&response);
            println!("{}", response.text().await.unwrap());
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

        let url = format!("{}/artifact/{}.tar.gz", self.url, hash);
        let response = reqwest::get(url).await?;

        if response.status() == 200 {
            let byte_stream = response
                .bytes_stream()
                .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e));
            let reader = tokio_util::io::StreamReader::new(byte_stream);

            let decompress_stream = GzipDecoder::new(reader.compat());

            let tar = async_tar::Archive::new(decompress_stream);
            tar.unpack(dst).await?;
        }

        Ok(())
    }
}
