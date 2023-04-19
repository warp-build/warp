use crate::model::ConcreteTarget;
use anyhow::*;
use futures::StreamExt;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::path::PathBuf;
use tokio::fs;
use tokio::io::AsyncWriteExt;
use tracing::{debug, instrument};

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct DownloadAction {
    pub url: String,
    pub sha256: String,
    pub output: PathBuf,
}

impl DownloadAction {
    #[instrument(name = "action::DownloadAction::run")]
    pub async fn run(
        &self,
        target: &ConcreteTarget,
        sandbox_root: &PathBuf,
    ) -> Result<(), anyhow::Error> {
        let out_path = sandbox_root.join(&self.output);

        let mut outfile = fs::File::create(&out_path).await?;

        let mut resp = reqwest::get(&self.url).await?.bytes_stream();

        let mut hasher = Sha256::new();
        while let Some(chunk) = resp.next().await {
            let mut chunk = chunk?;
            debug!("downloaded {} bytes", chunk.len());
            hasher.update(&chunk);
            outfile.write_all_buf(&mut chunk).await?;
        }

        let hash = format!("{:x}", hasher.finalize());
        debug!("download hashed {} == {}", &hash, &self.sha256);
        if hash == self.sha256 {
            Ok(())
        } else {
            Err(anyhow!(
                r#"The file we tried to download had a different SHA256 than what we expected. Is the checksum wrong?

We expected "{expected_sha}"

But found "{found_sha}"
"#,
                expected_sha = self.sha256,
                found_sha = hash,
            ))
        }
    }
}
