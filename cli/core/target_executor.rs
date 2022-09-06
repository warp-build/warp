use super::*;

pub struct TargetExecutor {
    local: LocalTargetExecutor,
    remote: RemoteTargetExecutor,
}

pub enum TargetExecutorError {}

impl TargetExecutor {
    pub async fn execute(&self, target: &ExecutableTarget) -> Result<(), TargetExecutorError> {
        todo!()
    }
}
