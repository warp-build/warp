use super::*;

pub trait TargetExecutor {
    type Error;
    async fn schedule(&self, target: &ExecutableTarget) -> Result<(), Self::Error>;
}
