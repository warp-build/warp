use anyhow::*;
use std::path::PathBuf;

#[derive(Debug, Clone, PartialEq)]
pub struct DownloadAction {
    pub url: String,
    pub sha1: String,
    pub output: PathBuf,
}

impl DownloadAction {
    #[tracing::instrument(name = "action::DownloadAction::run")]
    pub async fn run(self, sandbox_root: &PathBuf) -> Result<(), anyhow::Error> {
        todo!()
    }
}
