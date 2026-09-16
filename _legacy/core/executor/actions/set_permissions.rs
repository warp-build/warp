use anyhow::*;
use serde::{Deserialize, Serialize};
use std::os::unix::fs::PermissionsExt;
use std::path::PathBuf;
use tokio::fs;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct SetPermissionsAction {
    pub file: PathBuf,
    pub executable: bool,
}

impl SetPermissionsAction {
    #[tracing::instrument(name = "action::SetPermissionsAction::run")]
    pub async fn run(&self, sandbox_root: &PathBuf) -> Result<(), anyhow::Error> {
        #[cfg(not(target_os = "windows"))]
        {
            let file = fs::File::open(sandbox_root.join(&self.file)).await?;
            let meta = file.metadata().await?;
            let mut permissions = meta.permissions();

            if self.executable {
                permissions.set_mode(0o555)
            }

            file.set_permissions(permissions).await?;
        };

        Ok(())
    }
}
