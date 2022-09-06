use super::*;

pub struct LocalExecutor {
    store: Arc<Store>,
}

impl TargetExecutor for LocalExecutor {
    type Error = LocalExecutorError;

    async fn schedule(&self, target: &ExecutableTarget) -> Result<(), Error> {
        if self.store.has_manifest_for_target(&target) {
            return Ok(());
        }

        self.clean_folder().await?;
        self.copy_sources(&target).await?;
        self.copy_dependencies(&target).await?;
        self.execute_actions(&target).await?;
        self.validate_outputs(&target).await?;
        self.write_manifest(&target).await?;

        Ok(())
    }
}

mod tests {}
