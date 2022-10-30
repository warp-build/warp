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

    #[error(transparent)]
    DependencyManagerError(DependencyManagerError),

    #[error("Could not queue everything to be executed")]
    CannotRunEverything,
}

/// A BuildExecutor orchestrates the build process.
///
#[derive(Builder, Clone, Debug)]
pub struct BuildExecutor {
    artifact_store: Arc<Store>,

    build_coordinator: Arc<BuildCoordinator>,

    build_opts: BuildOpts,

    build_queue: Arc<BuildQueue>,

    build_results: Arc<BuildResults>,

    dependency_manager: Arc<DependencyManager>,

    event_channel: Arc<EventChannel>,

    label_registry: Arc<LabelRegistry>,

    label_resolver: Arc<LabelResolver>,

    rule_store: Arc<RuleStore>,

    target_executor: Arc<TargetExecutor>,

    /// The workspace this worker is currently executing.
    workspace: Workspace,
}

impl BuildExecutor {
    #[tracing::instrument(name = "BuildExecutor::new")]
    pub async fn new(
        workspace: Workspace,
        event_channel: Arc<EventChannel>,
        build_opts: BuildOpts,
    ) -> Result<Self, BuildExecutorError> {
        let label_registry = Arc::new(LabelRegistry::new());

        let build_results = Arc::new(BuildResults::new(label_registry.clone()));

        let build_coordinator = Arc::new(BuildCoordinator::new());

        let dependency_manager = Arc::new(
            DependencyManager::new(&workspace, label_registry.clone())
                .await
                .map_err(BuildExecutorError::DependencyManagerError)?,
        );

        let build_queue = Arc::new(BuildQueue::new(
            build_results.clone(),
            event_channel.clone(),
            workspace.clone(),
            label_registry.clone(),
            build_opts,
        ));

        let artifact_store = Arc::new(Store::new(&workspace, event_channel.clone()));

        let rule_store = Arc::new(RuleStore::new(&workspace));

        let label_resolver = Arc::new(LabelResolver::new(
            &workspace,
            artifact_store.clone(),
            build_results.clone(),
            event_channel.clone(),
            label_registry.clone(),
            dependency_manager.clone(),
        ));

        let target_executor = Arc::new(TargetExecutor::new(
            artifact_store.clone(),
            build_results.clone(),
            event_channel.clone(),
        ));

        Ok(BuildExecutor {
            artifact_store,
            build_coordinator,
            build_opts,
            build_queue,
            build_results,
            dependency_manager,
            event_channel,
            label_registry,
            label_resolver,
            rule_store,
            target_executor,
            workspace,
        })
    }

    #[tracing::instrument(name = "BuildExecutor::execute", skip(self))]
    pub async fn execute(&self, targets: &[Label]) -> Result<Vec<BuildResult>, BuildExecutorError> {
        if targets.is_empty() {
            self.event_channel
                .send(Event::BuildCompleted(std::time::Instant::now()));
            return Ok(vec![]);
        }

        let should_queue_everything = targets.iter().any(|t| t.is_all());
        if should_queue_everything && self.build_opts.goal.is_run() {
            self.event_channel
                .send(Event::BuildCompleted(std::time::Instant::now()));
            return Err(BuildExecutorError::CannotRunEverything);
        }

        let share_rule_executor_state = Arc::new(SharedRuleExecutorState::new(
            self.rule_store.clone(),
            self.build_results.clone(),
        ));

        let mut worker = BuildWorker::new(
            Role::MainWorker,
            self.build_coordinator.clone(),
            self.event_channel.clone(),
            self.build_queue.clone(),
            self.build_results.clone(),
            self.label_registry.clone(),
            self.label_resolver.clone(),
            self.target_executor.clone(),
            self.artifact_store.clone(),
            share_rule_executor_state.clone(),
            self.build_opts,
        )
        .map_err(BuildExecutorError::WorkerError)?;

        let mut worker_tasks = vec![];
        if self.build_opts.concurrency_limit > 0 {
            let worker_pool =
                tokio_util::task::LocalPoolHandle::new(self.build_opts.concurrency_limit);
            for worker_id in 1..self.build_opts.concurrency_limit {
                let sub_worker_span = trace_span!("BuildExecutor::sub_worker");

                let artifact_store = self.artifact_store.clone();
                let build_coordinator = self.build_coordinator.clone();
                let build_opts = self.build_opts;
                let build_queue = self.build_queue.clone();
                let build_results = self.build_results.clone();
                let event_channel = self.event_channel.clone();
                let label_registry = self.label_registry.clone();
                let label_resolver = self.label_resolver.clone();
                let target_executor = self.target_executor.clone();

                let share_rule_executor_state = share_rule_executor_state.clone();

                let thread = worker_pool.spawn_pinned(move || async move {
                    let mut worker = BuildWorker::new(
                        Role::HelperWorker(worker_id),
                        build_coordinator,
                        event_channel,
                        build_queue,
                        build_results,
                        label_registry,
                        label_resolver,
                        target_executor,
                        artifact_store,
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

        let targets: Vec<LabelId> = targets
            .iter()
            .map(|t| self.label_registry.register_label(t))
            .collect();

        if should_queue_everything {
            let queued_count = self
                .build_queue
                .queue_entire_workspace()
                .await
                .map_err(BuildExecutorError::QueueError)?;

            if queued_count == 0 {
                self.event_channel
                    .send(Event::EmptyWorkspace(std::time::Instant::now()));
                self.build_coordinator.signal_shutdown();
            }
        } else {
            for target in &targets {
                self.build_queue
                    .queue(Task {
                        label: *target,
                        goal: self.build_opts.goal,
                    })
                    .map_err(BuildExecutorError::QueueError)?;
            }
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

        // Collect all results using our LabelIds
        let mut results = vec![];
        for target in targets {
            if let Some(result) = self.build_results.get_build_result(target) {
                results.push(result)
            }
        }
        Ok(results)
    }

    pub fn manifests(&self) -> Vec<Arc<TargetManifest>> {
        self.build_results
            .get_results()
            .iter()
            .map(|e| e.target_manifest.to_owned())
            .collect()
    }
}
