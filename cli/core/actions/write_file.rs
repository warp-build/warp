use anyhow::*;
use std::path::PathBuf;

#[derive(Debug, Clone)]
pub struct WriteFileAction {
    pub contents: String,
    pub dst: PathBuf,
}

impl WriteFileAction {
    pub fn run(self, sandbox_root: &PathBuf) -> Result<(), anyhow::Error> {
        if let Some(parent) = self.dst.parent() {
            std::fs::create_dir_all(sandbox_root.join(parent))?;
        }
        std::fs::write(sandbox_root.join(&self.dst), &self.contents)
            .map(|_| ())
            .context(format!("Could not run action {:#?}", &self))
    }
}
