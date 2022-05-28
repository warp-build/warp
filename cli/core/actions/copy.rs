use anyhow::*;
use std::path::PathBuf;
use tokio::fs;

#[derive(Debug, Clone)]
pub struct CopyAction {
    pub src: PathBuf,
    pub dst: PathBuf,
}

impl CopyAction {
    #[tracing::instrument(name = "action::CopyAction::run")]
    pub async fn run(self, sandbox_root: &PathBuf) -> Result<(), anyhow::Error> {
        if let Some(parent) = self.dst.parent() {
            fs::create_dir_all(sandbox_root.join(parent)).await?;
        }
        fs::copy(sandbox_root.join(&self.src), sandbox_root.join(&self.dst))
            .await
            .map(|_| ())
            .context(format!("Could not run action {:#?}", &self))
    }
}
