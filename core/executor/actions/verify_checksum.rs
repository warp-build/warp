use crate::{code::SourceHasher, model::ConcreteTarget};
use anyhow::*;
use serde::{Deserialize, Serialize};
use std::path::PathBuf;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct VerifyChecksumAction {
    pub file: PathBuf,
    pub sha256: String,
}

impl VerifyChecksumAction {
    #[tracing::instrument(name = "action::VerifyChecksumAction::run")]
    pub async fn run(
        &self,
        target: &ConcreteTarget,
        sandbox_root: &PathBuf,
    ) -> Result<(), anyhow::Error> {
        let file = sandbox_root.join(&self.file);
        let hash = SourceHasher::hash(&file).await?;
        if hash == self.sha256 {
            Ok(())
        } else {
            Err(anyhow!(
                r#"The file we tried to download had a different SHA256 than what we expected. Is the checksum wrong?

We expected "{expected_sha}"

But found "{found_sha}"

If this is the right SHA256 you can fix this by changing the `sha256` key to:

{found_sha}
"#,
                expected_sha = self.sha256,
                found_sha = hash,
            ))
        }
    }
}
