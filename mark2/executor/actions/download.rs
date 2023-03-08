use crate::events::Event;
use crate::events::EventChannel;
use crate::sync::Arc;
use crate::Target;
use anyhow::*;
use crypto::digest::Digest;
use crypto::sha1::Sha1;
use futures::StreamExt;
use std::path::PathBuf;
use tokio::fs;
use tokio::io::AsyncReadExt;
use tokio::io::AsyncWriteExt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DownloadAction {
    pub url: String,
    pub sha1: String,
    pub output: PathBuf,
}

impl DownloadAction {
    #[tracing::instrument(name = "action::DownloadAction::run")]
    pub async fn run(
        &self,
        target: Target,
        sandbox_root: &PathBuf,
        event_channel: Arc<EventChannel>,
    ) -> Result<(), anyhow::Error> {
        let out_path = sandbox_root.join(&self.output);

        let mut outfile = fs::File::create(&out_path).await?;

        event_channel.send(Event::ArchiveDownloading {
            target: target.to_string(),
            url: self.url.clone(),
        });

        let mut resp = reqwest::get(&self.url).await?.bytes_stream();

        while let Some(chunk) = resp.next().await {
            outfile.write_all_buf(&mut chunk?).await?;
        }

        event_channel.send(Event::ArchiveVerifying(target.to_string()));

        let mut outfile = fs::File::open(out_path).await?;
        let mut hasher = Sha1::new();
        let mut contents: Vec<u8> =
            std::vec::Vec::with_capacity(outfile.metadata().await?.len() as usize);
        outfile.read_to_end(&mut contents).await?;
        hasher.input(&contents);
        let hash = hasher.result_str();
        if hash == self.sha1 {
            Ok(())
        } else {
            Err(anyhow!(
                r#"The file we tried to download had a different SHA-1 than what we expected. Is the checksum wrong?

We expected "{expected_sha}"

But found "{found_sha}"

If this is the right SHA-1 you can fix this in your Workspace.json file
under by changing the `sha1` key to this:

sha1 = "{found_sha}"
"#,
                expected_sha = self.sha1,
                found_sha = hash,
            ))
        }
    }
}