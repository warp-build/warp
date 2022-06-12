/// WARNING: He-man, stateful, multi-threaded programming happening below.
///
use super::Event;
use super::*;
use futures::StreamExt; 
use anyhow::anyhow;
use dashmap::DashMap;
use std::collections::HashMap;
use std::sync::Arc;
use thiserror::*;
use tracing::*;

/// A BuildExecutor orchestrates a local build starting from the target and
/// building dependencies as needed.
///
pub struct BuildExecutor {
    /// The workspace this worker is currently executing.
    pub workspace: Workspace,

    /// The shared state of what targets have been built across workers.
    /// This is used in the `LazyWorker` below to check if the dependencies have
    /// already been built or not.
    pub computed_targets: Arc<DashMap<Label, ComputedTarget>>,

    /// The shared state of what targets are being built at the moment.
    pub busy_targets: Arc<DashMap<Label, ()>>,

    /// The shared state of what targets are queued up.
    pub requested_targets: Arc<DashMap<Label, ()>>,

    /// The maount of workers to spawn.
    pub worker_limit: usize,
}

impl BuildExecutor {
    #[tracing::instrument(name = "BuildExecutor::from_workspace")]
    pub fn from_workspace(workspace: Workspace, worker_limit: usize) -> BuildExecutor {
        BuildExecutor {
            workspace,
            worker_limit,
            computed_targets: Arc::new(DashMap::new()),
            busy_targets: Arc::new(DashMap::new()),
            requested_targets: Arc::new(DashMap::new()),
        }
    }

    #[tracing::instrument(name = "BuildExecutor::build", skip(self))]
    pub async fn build(
        &self,
        target: Label,
        event_channel: Arc<EventChannel>,
    ) -> Result<Option<ComputedTarget>, anyhow::Error> {
        self.execute(&target, ExecutionMode::OnlyBuild, event_channel)
            .await?;
        Ok(self
            .computed_targets
            .get(&target)
            .map(|n| n.clone())
            .clone())
    }

    #[tracing::instrument(name = "BuildExecutor::execute", skip(self))]
    pub async fn execute(
        &self,
        target: &Label,
        mode: ExecutionMode,
        event_channel: Arc<EventChannel>,
    ) -> Result<(), anyhow::Error> {
        let build_queue = Arc::new(crossbeam::deque::Injector::new());
        let result_queue = Arc::new(crossbeam::deque::Injector::new());
        let toolchain_provides_map: Arc<DashMap<Label, HashMap<String, String>>> =
            Arc::new(DashMap::new());

        let worker_limit = self.worker_limit;
        debug!("Starting build executor with {} workers...", &worker_limit);

        let main_worker_span = trace_span!("BuildExecutor::main_worker").entered();

        let mut worker = LazyWorker::new(
            0,
            &self.workspace,
            build_queue.clone(),
            result_queue.clone(),
            self.computed_targets.clone(),
            self.busy_targets.clone(),
            self.requested_targets.clone(),
            event_channel.clone(),
            toolchain_provides_map.clone(),
            target.clone(),
        );

        let rules_span = trace_span!("LazyWorker::load_rules").or_current();
        {
            let _ = rules_span.enter();
            worker.load_rules().await?;
        };

        let main_worker_span = main_worker_span.exit();

        let mut worker_tasks = vec![];
        if worker_limit > 0 {
            let worker_pool = tokio_util::task::LocalPoolHandle::new(worker_limit);
            for worker_id in 1..worker_limit {
                let sub_worker_span = trace_span!("BuildExecutor::sub_worker");
                let build_queue = build_queue.clone();
                let result_queue = result_queue.clone();
                let target = target.clone();
                let workspace = self.workspace.clone();
                let computed_targets = self.computed_targets.clone();
                let busy_targets = self.busy_targets.clone();
                let requested_targets = self.requested_targets.clone();
                let event_channel = event_channel.clone();
                let toolchain_provides_map = toolchain_provides_map.clone();
                let thread = worker_pool.spawn_pinned(move || async move {
                    let mut worker = LazyWorker::new(
                        worker_id,
                        &workspace,
                        build_queue,
                        result_queue,
                        computed_targets,
                        busy_targets,
                        requested_targets,
                        event_channel,
                        toolchain_provides_map,
                        target,
                    );
                    worker.load_rules().await?;
                    worker.run(false, mode, worker_limit).instrument(sub_worker_span).await
                });
                worker_tasks.push(thread);
            }
        }
        let _span = main_worker_span.enter();
        let (_, result) = futures::future::join(
            futures::future::join_all(worker_tasks),
            worker.run(true, mode, worker_limit),
        )
        .await;

        result
    }
}

#[derive(Error, Debug)]
pub enum WorkerError {
    #[error(transparent)]
    RuleExecError(rule_exec_env::error::RuleExecError),

    #[error(transparent)]
    ComputedTargetError(computed_target::ComputedTargetError),

    #[error("Terminate")]
    Terminate,

    #[error(transparent)]
    Unknown(anyhow::Error),
}

/// A worker executing the build.
///
pub struct LazyWorker {
    pub id: usize,

    /// The workspace this worker is currently executing.
    pub workspace: Workspace,

    pub cache: LocalCache,

    pub rule_exec_env: RuleExecEnv,

    /// Targets already built.
    pub computed_targets: Arc<DashMap<Label, ComputedTarget>>,

    /// Targets currently being built.
    pub busy_targets: Arc<DashMap<Label, ()>>,

    /// Targets queued up.
    pub requested_targets: Arc<DashMap<Label, ()>>,

    /// The queue from which workers pull work.
    pub build_queue: Arc<crossbeam::deque::Injector<Label>>,

    pub result_queue: Arc<crossbeam::deque::Injector<Result<Label, WorkerError>>>,

    pub target: Label,

    pub target_count: usize,

    pub event_channel: Arc<EventChannel>,
}

impl LazyWorker {
    #[tracing::instrument(
        name = "LazyWorker::new",
        skip(workspace, build_queue, result_queue, computed_targets, busy_targets)
    )]
    pub fn new(
        id: usize,
        workspace: &Workspace,
        build_queue: std::sync::Arc<crossbeam::deque::Injector<Label>>,
        result_queue: std::sync::Arc<crossbeam::deque::Injector<Result<Label, WorkerError>>>,
        computed_targets: Arc<DashMap<Label, ComputedTarget>>,
        busy_targets: Arc<DashMap<Label, ()>>,
        requested_targets: Arc<DashMap<Label, ()>>,
        event_channel: Arc<EventChannel>,
        toolchain_provides_map: Arc<DashMap<Label, HashMap<String, String>>>,
        target: Label,
    ) -> LazyWorker {
        LazyWorker {
            id,
            rule_exec_env: RuleExecEnv::new(&workspace, toolchain_provides_map),
            cache: LocalCache::new(&workspace),
            workspace: workspace.clone(),
            computed_targets,
            busy_targets,
            requested_targets,
            build_queue,
            result_queue,
            event_channel,
            target,
            target_count: 0,
        }
    }

    #[tracing::instrument(name = "LazyWorker::queue", skip(self))]
    pub async fn queue(
        &mut self,
        target: Label,
        max_concurrency: usize,
    ) -> Result<(), anyhow::Error> {
        if target.is_all() {
            debug!("Queueing all targets...");
            let scanner = WorkspaceScanner::from_paths(&self.workspace.paths);
            let mut buildfiles = scanner.find_build_files(max_concurrency).await?;
            while let Some(build_file) = buildfiles.next().await {
                let buildfile = Buildfile::from_file(
                    &self.workspace.paths.workspace_root,
                    &build_file?,
                    self.rule_exec_env.rule_map.clone(),
                )?;
                for target in buildfile.targets {
                    self.event_channel.send(Event::QueuedTarget(target.label().clone()));
                    self.build_queue.push(target.label().clone());
                    self.target_count += 1;
                }
            }
            self.event_channel.send(Event::QueuedTargets(self.target_count));
            debug!("Queued {} targets...", self.target_count);
        } else {
            self.event_channel.send(Event::QueuedTarget(target.clone()));
            self.build_queue.push(target);
            debug!("Queued 1 target...");
        }
        Ok(())
    }

    #[tracing::instrument(name = "LazyWorker::run", skip(self))]
    pub async fn run(&mut self, is_main: bool, mode: ExecutionMode, max_concurrency: usize) -> Result<(), anyhow::Error> {
        if is_main {
            self.queue(self.target.clone(), max_concurrency).await?;
        }

        loop {
            let loop_span = trace_span!("LazyWorker::run_loop").or_current();
            let _span = loop_span.enter();

            // NOTE(@ostera): we don't want things to burn CPU cycles
            tokio::time::sleep(std::time::Duration::from_millis(1)).await;

            // If we are the main worker and there is a result, we can signal
            // the other workers to stop working and submit a BuildCompleted event
            if is_main {
                if let crossbeam::deque::Steal::Success(result) = self.result_queue.steal() {
                    self.result_queue.push(result);
                    self.event_channel.send(Event::BuildCompleted);
                    return Ok(());
                }

                if self.target.is_all() && self.computed_targets.len() >= self.target_count {
                    self.result_queue.push(Ok(self.target.clone()));
                    self.event_channel.send(Event::BuildCompleted);
                    return Ok(());
                }
            }

            // If we are not the main worker and ther is a result, we can stop working
            if !is_main && !self.result_queue.is_empty() {
                return Ok(());
            }

            if let crossbeam::deque::Steal::Success(label) = self.build_queue.steal() {
                // If the target is already computed or being computed, we can skip it
                if self.computed_targets.contains_key(&label)
                    || self.busy_targets.contains_key(&label)
                {
                    continue;
                }

                self.busy_targets.insert(label.clone(), ());
                self.requested_targets.remove(&label);

                match self.execute(&label, mode).await {
                    Ok(node_label) => {
                        self.busy_targets.remove(&label);
                        debug!(
                            "TARGET BUILT {} {} {} {} {}",
                            label.to_string(),
                            node_label.to_string(),
                            self.target.to_string(),
                            self.target_count,
                            self.computed_targets.len()
                        );

                        // We built the thing we wanted to build!
                        if node_label == self.target {
                            debug!("found the thing, pushing {:?}", node_label);
                            self.result_queue.push(Ok(node_label));
                            continue;
                        }

                        if self.target_count > 0 && self.computed_targets.len() == self.target_count
                        {
                            debug!("built everything, pushing {:?}", node_label);
                            self.result_queue.push(Ok(node_label));

                            continue;
                        }
                    }

                    Err(WorkerError::ComputedTargetError(
                        computed_target::ComputedTargetError::MissingDependencies { deps, .. },
                    ))
                    | Err(WorkerError::RuleExecError(
                        rule_exec_env::error::RuleExecError::MissingDependencies(
                            computed_target::ComputedTargetError::MissingDependencies {
                                deps, ..
                            },
                        ),
                    )) => {
                        self.busy_targets.remove(&label);
                        debug!("Missing {} dependencies", deps.len());
                        for dep in deps {
                            if self.computed_targets.contains_key(&dep)
                                || self.busy_targets.contains_key(&dep)
                                || self.requested_targets.contains_key(&dep)
                            {
                                continue;
                            }
                            self.requested_targets.insert(dep.clone(), ());
                            self.event_channel.send(Event::QueuedTarget(dep.clone()));
                            self.build_queue.push(dep);
                        }
                        debug!("Queueing {}", label.to_string());
                        self.requested_targets.insert(label.clone(), ());
                        self.event_channel.send(Event::QueuedTarget(label.clone()));
                        self.build_queue.push(label);
                    }
                    Err(err) => {
                        self.event_channel
                            .send(Event::BuildError(label.clone(), err));
                        self.result_queue.push(Err(WorkerError::Terminate));
                    }
                }
            }
        }
    }

    #[tracing::instrument(name = "LazyWorker::load_rules", skip(self))]
    async fn load_rules(&mut self) -> Result<(), anyhow::Error> {
        self.rule_exec_env.setup()?;
        let built_in_rules = zap_ext::TOOLCHAINS
            .iter()
            .chain(zap_ext::RULES.iter())
            .map(|(name, src)| (name.to_string(), src.to_string()));

        let custom_rules = (self.workspace.local_toolchains.iter())
            .chain(self.workspace.local_rules.iter())
            .map(|rule| {
                let name = format!("file://{}", rule.to_str().unwrap());
                let src = std::fs::read_to_string(rule).unwrap();
                (name, src)
            });

        for (name, src) in built_in_rules.chain(custom_rules) {
            self.rule_exec_env.load(&name, Some(src)).await?
        }

        Ok(())
    }

    #[tracing::instrument(name = "LazyWorker::execute", skip(self))]
    pub async fn execute(
        &mut self,
        label: &Label,
        mode: ExecutionMode,
    ) -> Result<Label, WorkerError> {
        let target = if let Some(toolchain) = (*self.rule_exec_env.toolchain_manager)
            .read()
            .unwrap()
            .get(&label.to_string())
        {
            toolchain.as_target().clone()
        } else {
            let buildfile = Buildfile::from_file(
                &self.workspace.paths.workspace_root,
                &self
                    .workspace
                    .paths
                    .workspace_root
                    .join(label.path())
                    .join(buildfile::ZAPFILE),
                self.rule_exec_env.rule_map.clone(),
            )
            .map_err(WorkerError::Unknown)?;

            buildfile
                .targets
                .iter()
                .find(|t| *t.label() == *label)
                .ok_or(anyhow!("Could not find target in: {:?}", &label.path()))
                .map_err(WorkerError::Unknown)?
                .clone()
        };
        self.execute_target(target, mode).await
    }

    #[tracing::instrument(name="LazyWorker::execute_target", skip(self), fields(zap.target = %target.label().to_string()))]
    pub async fn execute_target(
        &mut self,
        target: Target,
        mode: ExecutionMode,
    ) -> Result<Label, WorkerError> {
        let label = target.label().clone();

        self.rule_exec_env.clear();

        let find_node = |label| self.computed_targets.get(&label).map(|r| r.clone());
        let computed_target = ComputedTarget::from_target_with_deps(target, &find_node)
            .map_err(WorkerError::ComputedTargetError)?;

        let node = self
            .rule_exec_env
            .compute_target(computed_target, &find_node)
            .map_err(WorkerError::RuleExecError)?;

        let name = node.label().clone();

        self.event_channel.send(Event::BuildingTarget {
            label: label.clone(),
            rule_mnemonic: node.target.rule().mnemonic().to_string(),
        });

        match self
            .cache
            .is_cached(&node)
            .await
            .map_err(WorkerError::Unknown)?
        {
            CacheHitType::Global(cache_path) => {
                self.computed_targets.insert(label.clone(), node.clone());
                self.event_channel
                    .send(Event::CacheHit(label.clone(), cache_path));
                return Ok(label.clone());
            }
            CacheHitType::Local(cache_path) => {
                debug!("Skipping {}, but promoting outputs.", name.to_string());
                self.computed_targets.insert(label.clone(), node.clone());
                self.cache
                    .promote_outputs(&node, &self.workspace.paths.local_outputs_root)
                    .await
                    .map_err(WorkerError::Unknown)?;
                self.event_channel
                    .send(Event::CacheHit(label.clone(), cache_path));
                return Ok(label.clone());
            }
            CacheHitType::Miss { local_path, .. } => {
                self.event_channel
                    .send(Event::CacheMiss{ label: label.clone(), local_path });
            }
        }

        let result: Result<Label, WorkerError> = if node.target.is_local() {
            let mut sandbox = LocalSandbox::for_node(&self.workspace, node.clone());

            let result = {
                let find_node = |label| self.computed_targets.get(&label).map(|r| r.clone());
                sandbox
                    .run(&self.cache, &find_node, mode, self.event_channel.clone()).await
                    .map_err(WorkerError::Unknown)?
            };

            match result {
                ValidationStatus::Valid => {
                    self.computed_targets.insert(label.clone(), node.clone());
                    self.cache.save(&sandbox).await.map_err(WorkerError::Unknown)?;
                    self.cache
                        .promote_outputs(&node, &self.workspace.paths.local_outputs_root)
                        .await
                        .map_err(WorkerError::Unknown)?;
                    Ok(label.clone())
                },
                ValidationStatus::NoOutputs => {
                    if node.outs().is_empty() {
                        self.cache.save(&sandbox).await.map_err(WorkerError::Unknown)?;
                        self.computed_targets.insert(label.clone(), node.clone());
                        Ok(label.clone())
                    } else {
                        Err(anyhow!( "Expected {} outputs, but found none.", node.outs().len()))
                    }
                },
                ValidationStatus::Pending => Err(anyhow!(
                    "Node {} is somehow still pending...",
                    &name.to_string()
                )),
                ValidationStatus::Invalid {
                    expected_but_missing,
                    unexpected_but_present,
                    ..
                } => Err(
                    anyhow!("Node {} expected the following but missing outputs: {:?}\n\ninstead it found the following unexpected outputs: {:?}",
                        &name.to_string(), expected_but_missing, unexpected_but_present)),
            }
        } else {
            let res = node.execute(
                &self.workspace.paths.global_archive_root,
                &self.workspace.paths.global_cache_root,
                &self.workspace.paths.global_cache_root,
                ExecutionMode::OnlyBuild,
                self.event_channel.clone(),
            ).await;

            self.computed_targets.insert(label.clone(), node.clone());

            res.map(|_| label.clone())

        }.map_err(WorkerError::Unknown);

        self.event_channel
            .send(Event::TargetBuilt(node.label().clone()));

        result
    }
}
