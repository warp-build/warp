/// WARNING: He-man, stateful, multi-threaded programming happening below.
///
use super::*;
use anyhow::anyhow;
use dashmap::DashMap;
use log::*;
use std::sync::Arc;
use thiserror::*;

/// A LazyWorker orchestrates a local build starting from the target and
/// building things upwards from it, if they are not cached yet.
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

    /// The maount of workers to spawn.
    pub worker_limit: usize,
}

impl BuildExecutor {
    pub fn from_workspace(workspace: Workspace, worker_limit: usize) -> BuildExecutor {
        BuildExecutor {
            workspace,
            worker_limit,
            computed_targets: Arc::new(DashMap::new()),
            busy_targets: Arc::new(DashMap::new()),
        }
    }

    pub async fn run(&self, target: Label) -> Result<(), anyhow::Error> {
        self.execute(target, ExecutionMode::BuildAndRun).await
    }
    pub async fn build(&self, target: Label) -> Result<(), anyhow::Error> {
        self.execute(target, ExecutionMode::OnlyBuild).await
    }

    pub async fn execute(&self, target: Label, mode: ExecutionMode) -> Result<(), anyhow::Error> {
        let build_queue = Arc::new(crossbeam::deque::Injector::new());
        let result_queue = Arc::new(crossbeam::deque::Injector::new());

        let worker_limit = self.worker_limit;
        debug!("Starting build executor with {} workers...", &worker_limit);

        let mut worker = LazyWorker::new(
            0,
            &self.workspace,
            build_queue.clone(),
            result_queue.clone(),
            self.computed_targets.clone(),
            self.busy_targets.clone(),
            target.clone(),
        );
        worker.load_rules().await?;
        worker.queue(target.clone())?;

        crossbeam::scope(move |scope| {
            let mut worker_threads = vec![];
            for worker_id in 1..worker_limit {
                let build_queue = build_queue.clone();
                let result_queue = result_queue.clone();
                let target = target.clone();
                let thread = scope.spawn(move |_| {
                    let rt = tokio::runtime::Runtime::new().unwrap();
                    rt.block_on(async {
                        let mut worker = LazyWorker::new(
                            worker_id,
                            &self.workspace,
                            build_queue,
                            result_queue,
                            self.computed_targets.clone(),
                            self.busy_targets.clone(),
                            target,
                        );
                        worker.load_rules().await?;
                        worker.run(false, mode)
                    })
                });
                worker_threads.push(thread);
            }

            worker.run(true, mode)
        })
        .map_err(|_| anyhow!("Something went wrong when running the workers"))?
    }
}

#[derive(Error, Debug)]
pub enum WorkerError {
    #[error(transparent)]
    RuleExecError(rule_exec_env::error::RuleExecError),

    #[error(transparent)]
    ComputedTargetError(computed_target::ComputedTargetError),

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

    pub computed_targets: Arc<DashMap<Label, ComputedTarget>>,

    pub busy_targets: Arc<DashMap<Label, ()>>,

    /// The queue from which workers pull work.
    pub build_queue: std::sync::Arc<crossbeam::deque::Injector<Label>>,

    pub result_queue:
        std::sync::Arc<crossbeam::deque::Injector<Result<ComputedTarget, WorkerError>>>,

    pub target: Label,

    pub target_count: usize,
}

impl LazyWorker {
    pub fn new(
        id: usize,
        workspace: &Workspace,
        build_queue: std::sync::Arc<crossbeam::deque::Injector<Label>>,
        result_queue: std::sync::Arc<
            crossbeam::deque::Injector<Result<ComputedTarget, WorkerError>>,
        >,
        computed_targets: Arc<DashMap<Label, ComputedTarget>>,
        busy_targets: Arc<DashMap<Label, ()>>,
        target: Label,
    ) -> LazyWorker {
        debug!("[Worker={}] Initialized worker!", id);
        LazyWorker {
            id,
            rule_exec_env: RuleExecEnv::new(&workspace),
            cache: LocalCache::new(&workspace),
            workspace: workspace.clone(),
            computed_targets,
            busy_targets,
            build_queue,
            result_queue,
            target,
            target_count: 0,
        }
    }

    pub fn queue(&mut self, target: Label) -> Result<(), anyhow::Error> {
        if target.is_all() {
            trace!("Queueing all targets...");
            let scanner = WorkspaceScanner::from_paths(&self.workspace.paths);
            for build_file in scanner.find_build_files()? {
                let buildfile = Buildfile::from_file(
                    &self.workspace.paths.workspace_root,
                    &build_file,
                    &self.rule_exec_env.rule_map,
                )?;
                for target in buildfile.targets {
                    trace!("QUEUED {}", target.label().to_string());
                    self.build_queue.push(target.label().clone());
                    self.target_count += 1;
                }
            }
        } else {
            self.build_queue.push(target);
            self.target_count = 1;
        }
        trace!("Queued {} targets...", self.target_count);
        Ok(())
    }

    pub fn run(&mut self, is_main: bool, mode: ExecutionMode) -> Result<(), anyhow::Error> {
        debug!("[Worker={}] Running...", self.id);
        loop {
            std::thread::sleep(std::time::Duration::from_millis(1));
            if is_main {
                match self.result_queue.steal() {
                    crossbeam::deque::Steal::Success(result) => {
                        trace!("Receiving result: {:?}", result);
                        self.result_queue.push(result);
                        return Ok(());
                    }
                    _ => (),
                }
            }

            if !is_main && !self.result_queue.is_empty() {
                return Ok(());
            }

            if let crossbeam::deque::Steal::Success(label) = self.build_queue.steal() {
                if self.computed_targets.contains_key(&label) {
                    debug!(
                        "[Worker={}] ALREADY BUILT TARGET {}",
                        self.id,
                        label.to_string()
                    );
                    continue;
                }
                if self.busy_targets.contains_key(&label) {
                    debug!(
                        "[Worker={}] BEING BUILT TARGET {}",
                        self.id,
                        label.to_string()
                    );
                    continue;
                }

                if !is_main && mode == ExecutionMode::BuildAndRun && label == self.target {
                    self.build_queue.push(label);
                    continue;
                }

                self.busy_targets.insert(label.clone(), ());

                match self.execute(&label, mode) {
                    Ok(node) => {
                        self.busy_targets.remove(&label);
                        debug!(
                            "[Worker={}] TARGET {} BUILT",
                            self.id,
                            node.target.label().to_string()
                        );
                        // We built the thing we wanted to build!
                        if *node.target.label() == self.target
                            || self.computed_targets.len() == self.target_count
                        {
                            trace!("We're done! Pushing result: {:?}", &node);
                            self.result_queue.push(Ok(node.clone()));
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

                        debug!("[Worker={}] SKIPPED {}", self.id, label.to_string());
                        debug!("Missing {} dependencies", deps.len());
                        for dep in deps {
                            if self.computed_targets.contains_key(&label) {
                                continue;
                            }
                            if self.busy_targets.contains_key(&label) {
                                continue;
                            }
                            debug!("[Worker={}] WAITING FOR: {}", self.id, dep.to_string());
                            debug!("Queueing {:?}", &dep);
                            self.build_queue.push(dep);
                        }
                        debug!("Queueing {:?}", &label);
                        self.build_queue.push(label);
                    }
                    Err(err) => panic!(
                        "[Worker={}] Something has gone horribly wrong: {:?}",
                        self.id, err
                    ),
                }
            } else {
                trace!("[Worker={}] FAILED TO STEAL WORK", self.id);
            }
        }
    }

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
        debug!("[Worker={}] Loaded rules", self.id);

        debug!("[Worker={}] Reading toolchains", self.id);

        let toolchains = (*self.rule_exec_env.toolchain_manager)
            .read()
            .unwrap()
            .targets();

        for toolchain in toolchains {
            self.execute_target(toolchain, ExecutionMode::OnlyBuild)?;
        }

        debug!("[Worker={}] Toolchains ready!", self.id);

        Ok(())
    }

    pub fn execute(
        &mut self,
        label: &Label,
        mode: ExecutionMode,
    ) -> Result<ComputedTarget, WorkerError> {
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
                &self.rule_exec_env.rule_map,
            )
            .map_err(WorkerError::Unknown)?;
            debug!("Found buildfile {:?}", &buildfile);

            buildfile
                .targets
                .iter()
                .find(|t| *t.label() == *label)
                .ok_or(anyhow!("Could not find target in: {:?}", &label.path()))
                .map_err(WorkerError::Unknown)?
                .clone()
        };
        self.execute_target(target, mode)
    }

    pub fn execute_target(
        &mut self,
        target: Target,
        mode: ExecutionMode,
    ) -> Result<ComputedTarget, WorkerError> {
        let label = target.label().clone();
        self.rule_exec_env.clear();

        debug!("Executing {:?}", &label);

        let find_node = |label| self.computed_targets.get(&label).map(|r| r.clone());
        let computed_target = ComputedTarget::from_target_with_deps(target, &find_node)
            .map_err(WorkerError::ComputedTargetError)?;
        debug!("Sealing target {:?}", &label);
        let node = self
            .rule_exec_env
            .compute_target(computed_target, &find_node)
            .map_err(WorkerError::RuleExecError)?;

        debug!("[Worker={}] Building {}...", self.id, label.to_string());

        let name = node.label().clone();
        debug!("About to build {:?}...", name.to_string());
        debug!("with sources {:?}...", &node.srcs());
        debug!("with dependencies {:?}...", &node.deps());

        match self.cache.is_cached(&node).map_err(WorkerError::Unknown)? {
            CacheHitType::Global => {
                self.computed_targets.insert(label.clone(), node.clone());
                debug!("Skipping {}. Nothing to do.", name.to_string());
                return Ok(node);
            }
            CacheHitType::Local => {
                if node.target.kind() == TargetKind::Runnable && mode == ExecutionMode::BuildAndRun
                {
                    debug!("Skipping {}, we're in running mode.", name.to_string());
                } else {
                    debug!("Skipping {}, but promoting outputs.", name.to_string());
                    self.computed_targets.insert(label.clone(), node.clone());
                    self.cache
                        .promote_outputs(&node, &self.workspace.paths.local_outputs_root)
                        .map_err(WorkerError::Unknown)?;
                    return Ok(node);
                }
            }
            CacheHitType::Miss => {
                debug!("Cache miss! Proceeding to build...");
            }
        }

        let result = if node.target.is_local() {
            let mut sandbox = LocalSandbox::for_node(&self.workspace, &node);

            let result = {
                let find_node = |label| self.computed_targets.get(&label).map(|r| r.clone());
                sandbox
                    .run(&self.cache, &find_node, mode)
                    .map_err(WorkerError::Unknown)?
            };

            match result {
                ValidationStatus::Valid => {
                    self.computed_targets.insert(label.clone(), node.clone());
                    self.cache.save(&sandbox).map_err(WorkerError::Unknown)?;
                    sandbox.clear_sandbox().map_err(WorkerError::Unknown)?;
                    Ok(node)
                }
                ValidationStatus::NoOutputs if node.outs().is_empty() => {
                    self.computed_targets.insert(label.clone(), node.clone());
                    sandbox.clear_sandbox().map_err(WorkerError::Unknown)?;
                    Ok(node)
                }
                ValidationStatus::NoOutputs => Err(anyhow!(
                    "Expected {} outputs, but found none.",
                    node.outs().len()
                )),
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
            debug!("Building global target...");
            let res = node.execute(
                &self.workspace.paths.global_archive_root,
                &self.workspace.paths.global_cache_root,
                &self.workspace.paths.global_sandbox_root,
                ExecutionMode::OnlyBuild,
            );

            res.map(|_| node)

        }.map_err(WorkerError::Unknown);

        result
    }
}
