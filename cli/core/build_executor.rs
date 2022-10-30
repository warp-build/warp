use super::Event;
use super::*;
use futures::StreamExt;
use std::sync::Arc;
use thiserror::*;
use tokio::time::Instant;
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
    pub async fn execute(&self, targets: &[Label]) -> Result<(), BuildExecutorError> {
        if targets.is_empty() {
            self.event_channel
                .send(Event::BuildCompleted(std::time::Instant::now()));
            return Ok(());
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

        let mut worker_tasks = vec![];
        let worker_pool =
            tokio_util::task::LocalPoolHandle::new(self.build_opts.concurrency_limit + 1);

        if self.build_opts.concurrency_limit > 0 {
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

        let build_coordinator = self.build_coordinator.clone();
        let build_opts = self.build_opts;
        let build_queue = self.build_queue.clone();
        let build_results = self.build_results.clone();
        let event_channel = self.event_channel.clone();
        let label_registry = self.label_registry.clone();
        let root = self.workspace.paths.workspace_root.clone();
        let queuer_worker: tokio::task::JoinHandle<Result<(), BuildExecutorError>> = worker_pool
            .spawn_pinned(move || async move {
                if build_opts.experimental_file_mode {
                    let skip_patterns = {
                        let mut builder = globset::GlobSetBuilder::new();
                        for pattern in &[
                            "*target*",
                            "*_build*",
                            "*.warp*",
                            "*warp-outputs*",
                            "*.git*",
                        ] {
                            let glob = globset::Glob::new(pattern).unwrap();
                            builder.add(glob);
                        }
                        builder.build().unwrap()
                    };

                    let mut dirs = vec![root.clone()];
                    while let Some(dir) = dirs.pop() {
                        let mut read_dir = tokio::fs::read_dir(&dir).await.unwrap();

                        while let Ok(Some(entry)) = read_dir.next_entry().await {
                            let path = entry.path().clone();

                            if skip_patterns.is_match(&path) {
                                continue;
                            }

                            let file_type = entry.metadata().await.unwrap().file_type();
                            if file_type.is_dir() {
                                dirs.push(path.clone());
                                continue;
                            };

                            let path = path.strip_prefix(&root).unwrap().to_path_buf();

                            let label = label_registry
                                .register_label(&Label::builder().from_path(path).unwrap());

                            build_queue
                                .queue(Task {
                                    label,
                                    goal: build_opts.goal,
                                })
                                .unwrap();
                        }
                    }
                } else if should_queue_everything {
                    let queued_count = build_queue
                        .queue_entire_workspace()
                        .await
                        .map_err(BuildExecutorError::QueueError)?;

                    if queued_count == 0 {
                        event_channel.send(Event::EmptyWorkspace(std::time::Instant::now()));
                        build_coordinator.signal_shutdown();
                    }
                } else {
                    for target in &targets {
                        build_queue
                            .queue(Task {
                                label: *target,
                                goal: build_opts.goal,
                            })
                            .map_err(BuildExecutorError::QueueError)?;
                    }
                }

                build_results.mark_as_ready();

                Ok(())
            });

        let main_worker_span = trace_span!("BuildExecutor::main_worker");
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
        let main_worker = worker_pool.spawn_pinned(move || async move {
            let mut worker = BuildWorker::new(
                Role::MainWorker,
                build_coordinator.clone(),
                event_channel.clone(),
                build_queue.clone(),
                build_results.clone(),
                label_registry.clone(),
                label_resolver.clone(),
                target_executor.clone(),
                artifact_store.clone(),
                share_rule_executor_state.clone(),
                build_opts,
            )
            .map_err(BuildExecutorError::WorkerError)?;

            worker
                .setup_and_run()
                .instrument(main_worker_span)
                .await
                .map_err(BuildExecutorError::WorkerError)
        });

        let (helper_results, main_result, queuer_result) = futures::future::join3(
            futures::future::join_all(worker_tasks),
            main_worker,
            queuer_worker,
        )
        .await;

        for task_result in helper_results {
            task_result.unwrap()?;
        }
        queuer_result.unwrap()?;
        main_result.unwrap()?;

        Ok(())
    }

    pub fn get_results(&self) -> Vec<BuildResult> {
        self.build_results.get_results().to_vec()
    }
}
