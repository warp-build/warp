use crate::resolver::TargetRegistry;
use crate::rules::SharedJsContext;
use crate::store::DefaultStore;
use crate::sync::*;
use crate::worker::TaskResults;

#[derive(Debug, Clone)]
pub struct DefaultPlannerContext {
    artifact_store: Arc<DefaultStore>,
    target_registry: Arc<TargetRegistry>,
    task_results: Arc<TaskResults>,
}

impl DefaultPlannerContext {
    pub fn new(
        artifact_store: Arc<DefaultStore>,
        target_registry: Arc<TargetRegistry>,
        task_results: Arc<TaskResults>,
    ) -> Self {
        Self {
            artifact_store,
            target_registry,
            task_results,
        }
    }
}

impl From<DefaultPlannerContext> for SharedJsContext {
    fn from(value: DefaultPlannerContext) -> Self {
        SharedJsContext::default()
    }
}

impl From<DefaultPlannerContext> for () {
    fn from(_value: DefaultPlannerContext) -> Self {
        ()
    }
}
