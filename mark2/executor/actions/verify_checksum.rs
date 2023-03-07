use crate::events::{Event, EventChannel};
use crate::sync::Arc;
use crate::Target;
use anyhow::*;
use crypto::digest::Digest;
use crypto::sha1::Sha1;
use std::path::PathBuf;
use tokio::fs;
use tokio::io::AsyncReadExt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VerifyChecksumAction {
    pub file: PathBuf,
    pub sha1: String,
}

impl VerifyChecksumAction {
    #[tracing::instrument(name = "action::VerifyChecksumAction::run")]
    pub async fn run(
        &self,
        target: Target,
        sandbox_root: &PathBuf,
        event_channel: Arc<EventChannel>,
    ) -> Result<(), anyhow::Error> {
        event_channel.send(Event::ArchiveVerifying(target.to_string()));

        let mut outfile = fs::File::open(sandbox_root.join(&self.file)).await?;
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

If this is the right SHA-1 you can fix this by changing the `sha1` key to:

{found_sha}
"#,
                expected_sha = self.sha1,
                found_sha = hash,
            ))
        }
    }
}
