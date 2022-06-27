use anyhow::*;
use hyper::body::HttpBody;
use std::path::PathBuf;
use tokio::fs;
use tokio::io::AsyncWriteExt;

#[derive(Debug, Clone, PartialEq)]
pub struct DownloadAction {
    pub url: String,
    pub sha1: String,
    pub output: PathBuf,
}

impl DownloadAction {
    #[tracing::instrument(name = "action::DownloadAction::run")]
    pub async fn run(self, sandbox_root: &PathBuf) -> Result<(), anyhow::Error> {
        let mut outfile = fs::File::create(sandbox_root.join(self.output)).await?;

        let tls = hyper_tls::HttpsConnector::new();
        let client = hyper::Client::builder().build::<_, hyper::Body>(tls);
        let req = hyper::Request::get(self.url).body(hyper::Body::empty())?;
        let mut resp = client.request(req).await?;

        while let Some(chunk) = resp.body_mut().data().await {
            outfile.write_all_buf(&mut chunk?).await?;
        }

        Ok(())
    }
}
