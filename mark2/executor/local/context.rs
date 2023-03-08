use crate::store::DefaultStore;
use crate::sync::*;
use crate::worker::TaskResults;

#[derive(Debug, Clone)]
pub struct LocalExecutorContext {
    pub(crate) artifact_store: Arc<DefaultStore>,
    pub(crate) task_results: Arc<TaskResults>,
}

impl LocalExecutorContext {
    pub fn new(artifact_store: Arc<DefaultStore>, task_results: Arc<TaskResults>) -> Self {
        Self {
            artifact_store,
            task_results,
        }
    }
}
