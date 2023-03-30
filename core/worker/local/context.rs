use tracing::instrument;

use crate::code::CodeManager;
use crate::config::Config;
use crate::events::EventChannel;
use crate::executor::local::LocalExecutorContext;
use crate::planner::DefaultPlannerContext;
use crate::resolver::{Resolver, SignatureRegistry, TargetRegistry};
use crate::rules::RuleStore;
use crate::store::{DefaultStore, Store};
use crate::sync::Arc;
use crate::testing::TestMatcherRegistry;
use crate::tricorder::GrpcTricorder;
use crate::worker::coordinator::Coordinator;
use crate::worker::task_queue::TaskQueue;
use crate::worker::{Context, TaskRegistry, TaskResults};
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
#[derive(Clone)]
pub struct LocalSharedContext<R: Resolver, S: Store> {
    pub(crate) artifact_store: Arc<S>,
    pub(crate) coordinator: Arc<Coordinator>,
    pub(crate) event_channel: Arc<EventChannel>,
    pub(crate) resolver: Arc<R>,
    pub(crate) rule_store: Arc<RuleStore>,
    pub(crate) task_registry: Arc<TaskRegistry>,
    pub(crate) target_registry: Arc<TargetRegistry>,
    pub(crate) signature_registry: Arc<SignatureRegistry>,
    pub(crate) task_queue: Arc<TaskQueue>,
    pub(crate) task_results: Arc<TaskResults>,
    pub(crate) test_matcher_registry: Arc<TestMatcherRegistry>,
    pub(crate) workspace_manager: Arc<WorkspaceManager>,
    pub(crate) code_manager: Arc<CodeManager<S, GrpcTricorder>>,
}

impl<R, S> LocalSharedContext<R, S>
where
    R: Resolver,
    S: Store,
{
    #[instrument(
        name = "SharedContext::new",
        skip(
            task_registry,
            target_registry,
            signature_registry,
            test_matcher_registry,
            resolver,
            artifact_store,
            workspace_manager,
            task_results,
            code_manager,
        )
    )]
    pub fn new(
        config: Config,
        task_registry: Arc<TaskRegistry>,
        target_registry: Arc<TargetRegistry>,
        signature_registry: Arc<SignatureRegistry>,
        test_matcher_registry: Arc<TestMatcherRegistry>,
        resolver: R,
        artifact_store: Arc<S>,
        workspace_manager: Arc<WorkspaceManager>,
        task_results: Arc<TaskResults>,
        code_manager: Arc<CodeManager<S, GrpcTricorder>>,
    ) -> Self {
        let coordinator = Arc::new(Coordinator::new());

        let task_queue = Arc::new(TaskQueue::new(
            &config,
            task_results.clone(),
            task_registry.clone(),
            target_registry.clone(),
            signature_registry.clone(),
        ));

        let resolver = Arc::new(resolver);

        let rule_store = Arc::new(RuleStore::new(&config));

        Self {
            artifact_store,
            code_manager,
            coordinator,
            event_channel: config.event_channel(),
            resolver,
            rule_store,
            signature_registry,
            target_registry,
            task_queue,
            task_registry,
            task_results,
            test_matcher_registry,
            workspace_manager,
        }
    }
}

impl<R, S> Context for LocalSharedContext<R, S>
where
    R: Resolver,
    S: Store,
{
    fn results(&self) -> Arc<TaskResults> {
        self.task_results.clone()
    }
}

impl<R: Resolver> From<LocalSharedContext<R, DefaultStore>> for LocalExecutorContext {
    fn from(ctx: LocalSharedContext<R, DefaultStore>) -> Self {
        Self::new(ctx.artifact_store, ctx.task_results)
    }
}

impl<R: Resolver> From<LocalSharedContext<R, DefaultStore>> for DefaultPlannerContext {
    fn from(ctx: LocalSharedContext<R, DefaultStore>) -> Self {
        Self::new(
            ctx.task_registry,
            ctx.target_registry,
            ctx.signature_registry,
            ctx.task_results,
            ctx.rule_store,
            ctx.code_manager,
        )
    }
}

#[cfg(test)]
impl<R, S> From<LocalSharedContext<R, S>> for ()
where
    R: Resolver,
    S: Store,
{
    fn from(_value: LocalSharedContext<R, S>) {}
}
