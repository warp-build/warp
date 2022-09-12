use std::sync::Arc;

use super::*;
use thiserror::*;

pub struct TargetExecutor {
    local: LocalTargetExecutor,
    remote: RemoteTargetExecutor,
}

#[derive(Error, Debug)]
pub enum TargetExecutorError {}

impl TargetExecutor {
    pub fn new(store: Arc<Store>) -> Self {
        Self {
            local: LocalTargetExecutor::new(store),
            remote: RemoteTargetExecutor::new(),
        }
    }

    pub async fn execute(&self, _target: &ExecutableTarget) -> Result<(), TargetExecutorError> {
        todo!()
    }
}
