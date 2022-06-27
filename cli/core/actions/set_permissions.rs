use anyhow::*;
use std::path::PathBuf;

#[derive(Debug, Clone, PartialEq)]
pub struct SetPermissionsAction {
    pub file: PathBuf,
    pub executable: bool,
}

impl SetPermissionsAction {
    #[tracing::instrument(name = "action::SetPermissionsAction::run")]
    pub async fn run(self, sandbox_root: &PathBuf) -> Result<(), anyhow::Error> {
        todo!()
    }
}
