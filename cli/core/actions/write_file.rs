use anyhow::*;
use std::path::PathBuf;
use tokio::fs;

#[derive(Debug, Clone)]
pub struct WriteFileAction {
    pub contents: String,
    pub dst: PathBuf,
}

impl WriteFileAction {
    #[tracing::instrument(name = "action::WriteFileAction::run")]
    pub async fn run(self, sandbox_root: &PathBuf) -> Result<(), anyhow::Error> {
        if let Some(parent) = self.dst.parent() {
            fs::create_dir_all(sandbox_root.join(parent)).await?;
        }
        fs::write(sandbox_root.join(&self.dst), &self.contents)
            .await
            .map(|_| ())
            .context(format!("Could not run action {:#?}", &self))
    }
}
