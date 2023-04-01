use crate::resolver::TargetRegistry;
use crate::store::DefaultStore;
use crate::sync::*;
use crate::worker::TaskResults;

#[derive(Debug, Clone)]
pub struct LocalExecutorContext {
    pub(crate) artifact_store: Arc<DefaultStore>,
    pub(crate) task_results: Arc<TaskResults>,
    pub(crate) target_registry: Arc<TargetRegistry>,
}

impl LocalExecutorContext {
    pub fn new(
        artifact_store: Arc<DefaultStore>,
        task_results: Arc<TaskResults>,
        target_registry: Arc<TargetRegistry>,
    ) -> Self {
        Self {
            artifact_store,
            task_results,
            target_registry,
        }
    }
}
