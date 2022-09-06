use super::*;

pub struct RemoteTargetExecutor {
    api: Api,
}

pub enum RemoteExecutorError {}

impl RemoteTargetExecutor {
    pub async fn schedule(&self, target: &ExecutableTarget) -> Result<(), RemoteExecutorError> {
        let job_id = self.api.schedule_target(&target).await?;
        self.api.poll_job(job_id).await
    }
}

mod tests {}
