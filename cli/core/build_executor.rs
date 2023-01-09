use super::Event;
use super::*;
use futures::StreamExt;
use std::sync::Arc;
use thiserror::*;
use tokio_util::task::LocalPoolHandle;
use tracing::*;

#[derive(Error, Debug)]
pub enum BuildExecutorError {
    #[error(transparent)]
    WorkerError(build_worker::BuildWorkerError),

    #[error(transparent)]
    QueueError(build_queue::QueueError),

    #[error(transparent)]
    LabelError(LabelError),

    #[error(transparent)]
    DependencyManagerError(DependencyManagerError),

    #[error(transparent)]
    ToolchainManagerError(ToolchainManagerError),

    #[error("Could not queue everything to be executed")]
    CannotRunEverything,

    #[error(transparent)]
    WorkspaceScannerError(WorkspaceScannerError),
}

/// A BuildExecutor orchestrates the build process.
///
#[derive(Builder, Clone, Debug)]
pub struct BuildExecutor {
    pub(crate) archive_manager: Arc<ArchiveManager>,

    pub(crate) artifact_store: Arc<ArtifactStore>,

    pub(crate) build_coordinator: Arc<BuildCoordinator>,

    pub(crate) build_opts: BuildOpts,

    pub(crate) build_queue: Arc<BuildQueue>,

    pub(crate) build_results: Arc<BuildResults>,

    pub(crate) dependency_manager: Arc<DependencyManager>,

    pub(crate) event_channel: Arc<EventChannel>,

    pub(crate) label_registry: Arc<LabelRegistry>,

    pub(crate) label_resolver: Arc<LabelResolver>,

    pub(crate) rule_store: Arc<RuleStore>,

    pub(crate) source_manager: Arc<SourceManager>,

    pub(crate) signature_store: Arc<SignatureStore>,

    pub(crate) target_executor: Arc<TargetExecutor>,

    worker_pool: LocalPoolHandle,

    /// The workspace this worker is currently executing.
    pub(crate) workspace: Workspace,
}

impl BuildExecutor {
    #[tracing::instrument(name = "BuildExecutor::new")]
    pub async fn new(
        workspace: Workspace,
        event_channel: Arc<EventChannel>,
        build_opts: BuildOpts,
    ) -> Result<Self, BuildExecutorError> {
        let archive_manager = Arc::new(ArchiveManager::new(&workspace, event_channel.clone()));

        let label_registry = Arc::new(LabelRegistry::new());

        let build_results = Arc::new(BuildResults::new(label_registry.clone()));

        let build_coordinator = Arc::new(BuildCoordinator::new());

        let toolchain_manager = Arc::new(
            ToolchainManager::new(&workspace, label_registry.clone())
                .map_err(BuildExecutorError::ToolchainManagerError)?,
        );

        let analyzer_service_manager = Arc::new(AnalyzerServiceManager::new(
            &workspace,
            label_registry.clone(),
            build_results.clone(),
            event_channel.clone(),
            build_opts,
        ));

        let resolver_service_manager = Arc::new(ResolverServiceManager::new(
            &workspace,
            label_registry.clone(),
            build_results.clone(),
            event_channel.clone(),
            build_opts,
        ));

        let dependency_manager = Arc::new(
            DependencyManager::new(&workspace, label_registry.clone())
                .await
                .map_err(BuildExecutorError::DependencyManagerError)?,
        );

        let build_queue = Arc::new(BuildQueue::new(
            build_results.clone(),
            label_registry.clone(),
            event_channel.clone(),
        ));

        let artifact_store = Arc::new(ArtifactStore::new(&workspace, event_channel.clone()));

        let rule_store = Arc::new(RuleStore::new(&workspace));

        let signature_store = Arc::new(SignatureStore::new(
            &workspace,
            build_results.clone(),
            event_channel.clone(),
            artifact_store.clone(),
            label_registry.clone(),
            analyzer_service_manager.clone(),
            dependency_manager.clone(),
            build_opts,
        ));

        let source_manager = Arc::new(SourceManager::new(
            &workspace,
            build_results.clone(),
            event_channel.clone(),
            artifact_store.clone(),
            label_registry.clone(),
            analyzer_service_manager.clone(),
            dependency_manager.clone(),
            build_opts,
        ));

        let label_resolver = Arc::new(LabelResolver::new(
            &workspace,
            artifact_store.clone(),
            build_results.clone(),
            event_channel.clone(),
            label_registry.clone(),
            dependency_manager.clone(),
            source_manager.clone(),
            signature_store.clone(),
            toolchain_manager.clone(),
            resolver_service_manager.clone(),
            build_opts,
        ));

        let target_executor = Arc::new(TargetExecutor::new(
            artifact_store.clone(),
            build_results.clone(),
            event_channel.clone(),
        ));

        let worker_pool = LocalPoolHandle::new({
            let max = num_cpus::get();
            let curr = build_opts.concurrency_limit + 2;
            curr.min(max)
        });

        Ok(BuildExecutor {
            archive_manager,
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
            signature_store,
            source_manager,
            target_executor,
            worker_pool,
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

        let queuer_worker = self.spawn_queuer_worker(targets);
        let main_worker = self.spawn_main_worker();

        // NOTE(@ostera): special case -- if we use the `-w0` flag, we run first the queuer, then
        // the main worker, and we finish. No concurrency.
        if self.build_opts.concurrency_limit == 0 {
            queuer_worker.await.unwrap()?;
            main_worker.await.unwrap()?;
            return Ok(());
        }

        // NOTE(@ostera): we are leaving 2 threads for the main worker and the queuer
        let mut worker_tasks = vec![];
        for worker_id in 2..self.worker_pool.num_threads() {
            worker_tasks.push(self.spawn_helper(worker_id));
        }

        let (queuer_result, main_result, helper_results) = futures::future::join3(
            queuer_worker,
            main_worker,
            futures::future::join_all(worker_tasks),
        )
        .await;

        queuer_result.unwrap()?;
        for task_result in helper_results {
            task_result.unwrap()?;
        }
        main_result.unwrap()?;

        Ok(())
    }

    pub fn get_results(&self) -> Vec<BuildResult> {
        self.build_results.get_results()
    }

    fn spawn_helper(
        &self,
        worker_id: usize,
    ) -> tokio::task::JoinHandle<Result<(), BuildExecutorError>> {
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
        let source_manager = self.source_manager.clone();

        let shared_rule_executor_state = Arc::new(SharedRuleExecutorState::new(
            self.rule_store.clone(),
            self.build_results.clone(),
        ));

        self.worker_pool.spawn_pinned(move || async move {
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
                shared_rule_executor_state,
                source_manager,
                build_opts,
            )
            .map_err(BuildExecutorError::WorkerError)?;

            worker
                .setup_and_run()
                .instrument(sub_worker_span)
                .await
                .map_err(BuildExecutorError::WorkerError)
        })
    }

    fn spawn_main_worker(&self) -> tokio::task::JoinHandle<Result<(), BuildExecutorError>> {
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
        let source_manager = self.source_manager.clone();

        let shared_rule_executor_state = Arc::new(SharedRuleExecutorState::new(
            self.rule_store.clone(),
            self.build_results.clone(),
        ));

        self.worker_pool.spawn_pinned(move || async move {
            let mut worker = BuildWorker::new(
                Role::MainWorker,
                build_coordinator,
                event_channel,
                build_queue,
                build_results,
                label_registry,
                label_resolver,
                target_executor,
                artifact_store,
                shared_rule_executor_state,
                source_manager,
                build_opts,
            )
            .map_err(BuildExecutorError::WorkerError)?;

            worker
                .setup_and_run()
                .instrument(main_worker_span)
                .await
                .map_err(BuildExecutorError::WorkerError)
        })
    }

    fn spawn_queuer_worker(
        &self,
        labels: &[Label],
    ) -> tokio::task::JoinHandle<Result<(), BuildExecutorError>> {
        let should_queue_everything = labels.iter().any(|t| t.is_all());
        let labels: Vec<Label> = labels.to_vec();

        let build_coordinator = self.build_coordinator.clone();
        let build_opts = self.build_opts;
        let build_queue = self.build_queue.clone();
        let build_results = self.build_results.clone();
        let event_channel = self.event_channel.clone();
        let label_registry = self.label_registry.clone();

        let root = self.workspace.paths.workspace_root.clone();
        let workspace = self.workspace.clone();
        let workspace_scanner = self.workspace.scanner();

        self.worker_pool.spawn_pinned(move || async move {
            if should_queue_everything {
                event_channel.send(Event::QueueingWorkspace);
                let skip_patterns = {
                    let mut builder = globset::GlobSetBuilder::new();
                    for pattern in &workspace.ignore_patterns {
                        let glob = globset::Glob::new(&format!("*{}*", pattern)).unwrap();
                        builder.add(glob);
                    }
                    for pattern in &["*/.warp*", "*warp-outputs*", "*.git*"] {
                        let glob = globset::Glob::new(pattern).unwrap();
                        builder.add(glob);
                    }
                    builder.build().unwrap()
                };

                let mut target_count = 0;
                let mut dirs = vec![root.clone()];
                while let Some(dir) = dirs.pop() {
                    let mut read_dir = tokio::fs::read_dir(&dir).await.unwrap();

                    while let Ok(Some(entry)) = read_dir.next_entry().await {
                        let path = entry.path().clone();

                        if skip_patterns.is_match(&path) {
                            continue;
                        }

                        if tokio::fs::read_dir(&path).await.is_ok() {
                            dirs.push(path.clone());
                            continue;
                        };

                        let path = path.strip_prefix(&root).unwrap().to_path_buf();

                        let local_label = Label::local_builder()
                            .workspace(root.clone())
                            .name(Some(
                                path.file_name().unwrap().to_string_lossy().to_string(),
                            ))
                            .file(path)
                            .build()
                            .map_err(BuildExecutorError::LabelError)?;

                        let label: Label = local_label.into();
                        let label = label_registry.register_label(label);

                        build_queue
                            .queue(Task {
                                label,
                                goal: build_opts.goal,
                            })
                            .unwrap();

                        target_count += 1;
                    }
                }

                let mut buildfiles = workspace_scanner
                    .find_build_files(build_opts.concurrency_limit)
                    .await
                    .map_err(BuildExecutorError::WorkspaceScannerError)?;

                while let Some(Ok(buildfile_path)) = buildfiles.next().await {
                    let buildfile =
                        SignaturesFile::read(&buildfile_path, &workspace.paths.workspace_root)
                            .await;

                    match buildfile {
                        Err(err) => {
                            event_channel.send(Event::BadBuildfile {
                                buildfile: buildfile_path,
                                error: err,
                            });
                            continue;
                        }
                        Ok(buildfile) => {
                            for signature in buildfile.signatures {
                                if build_opts.goal.includes(&signature) {
                                    let label = label_registry.register_label(&signature.name);
                                    let goal = build_opts.goal;

                                    build_queue
                                        .queue(Task { label, goal })
                                        .map_err(BuildExecutorError::QueueError)?;

                                    target_count += 1;
                                }
                            }
                        }
                    }
                }

                event_channel.send(Event::QueuedTargets(target_count as u64));

                if target_count == 0 {
                    event_channel.send(Event::EmptyWorkspace(std::time::Instant::now()));
                    build_coordinator.signal_shutdown();
                }
            } else {
                for label in labels {
                    let label = if label.is_file() {
                        if let Ok(meta) =
                            tokio::fs::metadata(label.get_local().unwrap().file()).await
                        {
                            if meta.file_type().is_dir() {
                                label.to_abstract().unwrap()
                            } else {
                                label.clone()
                            }
                        } else {
                            label.clone()
                        }
                    } else {
                        label.clone()
                    };

                    let label = label_registry.register_label(label);

                    build_queue
                        .queue(Task {
                            label,
                            goal: build_opts.goal,
                        })
                        .map_err(BuildExecutorError::QueueError)?;
                }
            }

            build_results.mark_as_ready();

            Ok(())
        })
    }

    pub fn clear_results(&self) {
        self.build_results.clear_results();
    }
}
