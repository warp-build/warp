use super::*;
use thiserror::*;

pub struct RemoteTargetExecutor {}

#[derive(Error, Debug)]
pub enum RemoteExecutorError {}

impl RemoteTargetExecutor {
    pub fn new() -> Self {
        Self {}
    }

    pub async fn schedule(&self, target: &ExecutableTarget) -> Result<(), RemoteExecutorError> {
        /*
        let job_id = self.api.schedule_target(&target).await?;
        self.api.poll_job(job_id).await
        */
        Ok(())
    }
}

mod tests {}
