use super::*;
use crate::config::Config;
use crate::events::EventChannel;
use crate::planner::DefaultPlannerContext;
use crate::resolver::{Resolver, TargetRegistry};
use crate::sync::Arc;
use crate::workspace::WorkspaceManager;

/// A shared execution context for workers. This includes all of subsystems that need to be
/// available for workers to execute their work correctly:
/// * the Task Queue from which they pull tasks to work on
/// * the Task Results where they can save the results of their work
/// * the Target Registry where they can look up the specific targets that they need to work on
/// * a Coordinator that helps initialize the shutdown sequence
/// * an Event Channel where they can report events
/// * a Workspace Manager to get information about the current workspace
///
#[derive(Debug, Clone)]
pub struct LocalSharedContext<R: Resolver> {
    pub(crate) coordinator: Arc<Coordinator>,
    pub(crate) event_channel: Arc<EventChannel>,
    pub(crate) target_registry: Arc<TargetRegistry>,
    pub(crate) task_queue: Arc<TaskQueue>,
    pub(crate) task_results: Arc<TaskResults>,
    pub(crate) workspace_manager: Arc<WorkspaceManager>,
    pub(crate) resolver: Arc<R>,
}

impl<R: Resolver> LocalSharedContext<R> {
    #[tracing::instrument(name = "SharedContext::new", skip(event_channel, resolver))]
    pub fn new(event_channel: Arc<EventChannel>, options: Config, resolver: R) -> Self {
        let workspace_manager = Arc::new(WorkspaceManager::new());

        let coordinator = Arc::new(Coordinator::new());

        let target_registry = Arc::new(TargetRegistry::new());

        let task_results = Arc::new(TaskResults::new(target_registry.clone()));

        let task_queue = Arc::new(TaskQueue::new(
            task_results.clone(),
            target_registry.clone(),
            event_channel.clone(),
        ));

        let resolver = Arc::new(resolver);

        Self {
            coordinator,
            event_channel,
            target_registry,
            resolver,
            task_queue,
            task_results,
            workspace_manager,
        }
    }
}

impl<R: Resolver> Context for LocalSharedContext<R> {
    fn results(&self) -> Arc<TaskResults> {
        self.task_results.clone()
    }
}

impl<R: Resolver> From<LocalSharedContext<R>> for DefaultPlannerContext {
    fn from(_value: LocalSharedContext<R>) -> Self {
        Self
    }
}
