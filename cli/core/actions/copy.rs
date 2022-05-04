use anyhow::*;
use std::path::PathBuf;

#[derive(Debug, Clone)]
pub struct CopyAction {
    pub src: PathBuf,
    pub dst: PathBuf,
}

impl CopyAction {
    pub fn run(self) -> Result<(), anyhow::Error> {
        if let Some(parent) = self.dst.parent() {
            std::fs::create_dir_all(parent)?;
        }
        std::fs::copy(&self.src, &self.dst)
            .map(|_| ())
            .context(format!("Could not run action {:#?}", &self))
    }
}
