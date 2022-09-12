use super::*;
use std::sync::Arc;

pub struct LocalTargetExecutor {
    store: Arc<Store>,
}
pub enum LocalExecutorError {}

impl LocalTargetExecutor {
    pub fn new(store: Arc<Store>) -> Self {
        Self { store }
    }

    pub async fn execute(&self, target: &ExecutableTarget) -> Result<(), LocalExecutorError> {
        /*
        if self.store.has_manifest_for_target(&target) {
            return Ok(());
        }

        self.clean_folder().await?;
        self.copy_sources(&target).await?;
        self.copy_dependencies(&target).await?;
        self.execute_actions(&target).await?;
        self.validate_outputs(&target).await?;
        self.write_manifest(&target).await?;
        */

        Ok(())
    }
}

mod tests {}
