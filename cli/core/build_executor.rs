use super::Event;
use super::*;
use std::sync::Arc;
use thiserror::*;
use tracing::*;

#[derive(Error, Debug)]
pub enum BuildExecutorError {
    #[error(transparent)]
    WorkerError(build_worker::BuildWorkerError),

    #[error(transparent)]
    QueueError(build_queue::QueueError),
}

/// A BuildExecutor orchestrates a local build starting from the target and
/// building dependencies as needed.
///
pub struct BuildExecutor {
    /// The workspace this worker is currently executing.
    pub workspace: Workspace,

    pub results: Arc<BuildResults>,

    pub coordinator: Arc<BuildCoordinator>,

    /// The amount of workers to spawn.
    pub worker_limit: usize,
}

impl BuildExecutor {
    #[tracing::instrument(name = "BuildExecutor::from_workspace")]
    pub fn from_workspace(workspace: Workspace, worker_limit: usize) -> BuildExecutor {
        BuildExecutor {
            workspace,
            worker_limit,
            results: Arc::new(BuildResults::new()),
            coordinator: Arc::new(BuildCoordinator::new()),
        }
    }

    #[tracing::instrument(name = "BuildExecutor::build", skip(self))]
    pub async fn build(
        &self,
        target: Label,
        event_channel: Arc<EventChannel>,
        build_opts: BuildOpts,
    ) -> Result<Option<(TargetManifest, ExecutableTarget)>, BuildExecutorError> {
        let worker_limit = self.worker_limit;
        debug!("Starting build executor with {} workers...", &worker_limit);

        let build_queue = Arc::new(BuildQueue::new(
            target.clone(),
            self.results.clone(),
            event_channel.clone(),
            self.workspace.clone(),
        ));
        let store = Arc::new(Store::new(&self.workspace));
        let rule_store = Arc::new(RuleStore::new(&self.workspace));
        let label_resolver = Arc::new(LabelResolver::new(&self.workspace, store.clone()));
        let target_executor = Arc::new(TargetExecutor::new(store.clone(), event_channel.clone()));

        let share_rule_executor_state = Arc::new(SharedRuleExecutorState::new(rule_store.clone()));

        let mut worker = BuildWorker::new(
            Role::MainWorker,
            target.clone(),
            self.coordinator.clone(),
            event_channel.clone(),
            build_queue.clone(),
            self.results.clone(),
            label_resolver.clone(),
            target_executor.clone(),
            store.clone(),
            share_rule_executor_state.clone(),
            build_opts,
        )
        .map_err(BuildExecutorError::WorkerError)?;

        let mut worker_tasks = vec![];
        if worker_limit > 0 {
            let worker_pool = tokio_util::task::LocalPoolHandle::new(worker_limit);
            for worker_id in 1..worker_limit {
                let sub_worker_span = trace_span!("BuildExecutor::sub_worker");
                let build_coordinator = self.coordinator.clone();
                let build_queue = build_queue.clone();
                let build_results = self.results.clone();
                let event_channel = event_channel.clone();
                let label_resolver = label_resolver.clone();
                let target_executor = target_executor.clone();
                let target = target.clone();
                let store = store.clone();
                let build_opts = build_opts;
                let share_rule_executor_state = share_rule_executor_state.clone();

                let thread = worker_pool.spawn_pinned(move || async move {
                    let mut worker = BuildWorker::new(
                        Role::HelperWorker(worker_id),
                        target,
                        build_coordinator,
                        event_channel,
                        build_queue,
                        build_results,
                        label_resolver,
                        target_executor,
                        store,
                        share_rule_executor_state,
                        build_opts,
                    )
                    .map_err(BuildExecutorError::WorkerError)?;

                    worker
                        .setup_and_run()
                        .instrument(sub_worker_span)
                        .await
                        .map_err(BuildExecutorError::WorkerError)
                });
                worker_tasks.push(thread);
            }
        }

        if target.is_all() {
            let queued_count = build_queue
                .queue_entire_workspace(worker_limit, build_opts.target_filter)
                .await
                .map_err(BuildExecutorError::QueueError)?;

            if queued_count == 0 {
                event_channel.send(Event::EmptyWorkspace(std::time::Instant::now()));
                self.coordinator.signal_shutdown();
            }
        } else {
            build_queue
                .queue(target.clone())
                .map_err(BuildExecutorError::QueueError)?;
        }

        let _span = trace_span!("BuildExecutor::main_worker").entered();

        futures::future::join(futures::future::join_all(worker_tasks), async {
            worker
                .setup_and_run()
                .await
                .map_err(BuildExecutorError::WorkerError)
        })
        .await
        .1?;

        Ok(self.results.get_computed_target(&target))
    }

    pub fn manifests(&self) -> Vec<TargetManifest> {
        self.results.get_all_manifests()
    }
}
