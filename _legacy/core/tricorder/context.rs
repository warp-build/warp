use crate::resolver::TargetRegistry;
use crate::sync::Arc;
use crate::worker::TaskRegistry;

#[derive(Debug, Clone)]
pub struct TricorderContext {
    pub target_registry: Arc<TargetRegistry>,
    pub task_registry: Arc<TaskRegistry>,
}

impl TricorderContext {
    pub fn new(target_registry: Arc<TargetRegistry>, task_registry: Arc<TaskRegistry>) -> Self {
        Self {
            target_registry,
            task_registry,
        }
    }
}
