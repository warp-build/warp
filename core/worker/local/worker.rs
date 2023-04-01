use crate::events::event::WorkerEvent;
use crate::executor::{ExecutionFlow, Executor, ExecutorError};
use crate::model::{ExecutableSpec, ExecutionEnvironment, SignatureId, UnregisteredTask};
use crate::planner::{Planner, PlannerError, PlanningFlow};
use crate::resolver::{ResolutionFlow, Resolver, ResolverError};
use crate::store::{ArtifactManifest, Store};
use crate::worker::task_queue::TaskQueueError;
use crate::worker::{Role, Task, Worker, WorkerError};
use crate::{Goal, Target};
use async_trait::async_trait;
use thiserror::*;
use tokio::fs;
use tracing::*;

use super::LocalSharedContext;

#[cfg_attr(doc, aquamarine::aquamarine)]
/// A Local build execution worker.
///
/// The `LocalWorker` takes care of running one-task at a time out of the `TaskQueue` that lives in
/// the `LocalSharedContext`. It contains a series of strategies for resolving, planning,a dn
/// executing work.
///
/// It orchestrates its lifecycle by checking with the `Coordinator` (also in the `LocalSharedContext`).
///
/// It's flow goes:
///
/// ```mermaid
/// graph TD
///   Idle -->|run| loop
///   loop -->|has next task| execute
///
///   execute
///   --> resolve_target
///   --> plan_signature
///   --> execute_plan
///   --> cache_results
///   --> should_stop
///
///   loop -->|no next task| should_stop
///   should_stop -->|is main && has all results| signal_shutdown
///   should_stop -->|no| sleep
///   sleep -->|timeout| loop
/// ```
///
pub struct LocalWorker<R: Resolver, P: Planner, E: Executor, S: Store> {
    role: Role,
    ctx: LocalSharedContext<R, S>,
    planner: P,
    executor: E,
    env: ExecutionEnvironment,
}

pub enum WorkerFlow {
    Complete {
        task: Task,
        executable_spec: ExecutableSpec,
        artifact_manifest: ArtifactManifest,
    },

    /// Used to permanently skip a task. It will not be requeued.
    Skipped(Task),

    /// Used to requeue the current task after queueing all dependencies.
    QueueDeps { deps: Vec<Task> },

    /// Replace the current task with new tasks that will build these signatures instead.
    ///
    /// This happens when the current task does not have a signature associated to it, and through
    /// the Resolution phase we find that there are many signatures to be built.
    ///
    SwapWithSignatures { signature_ids: Vec<SignatureId> },
}

#[async_trait(?Send)]
impl<R, P, PCtx, E, ECtx, S> Worker for LocalWorker<R, P, E, S>
where
    R: Resolver,
    P: Planner<Context = PCtx>,
    PCtx: From<LocalSharedContext<R, S>>,
    E: Executor<Context = ECtx>,
    ECtx: From<LocalSharedContext<R, S>>,
    S: Store,
{
    type Context = LocalSharedContext<R, S>;

    fn new(role: Role, ctx: Self::Context) -> Result<Self, WorkerError> {
        let env = ExecutionEnvironment::new();
        let planner = P::new(ctx.clone().into())?;
        let executor = E::new(ctx.clone().into())?;
        Ok(Self {
            role,
            ctx,
            env,
            planner,
            executor,
        })
    }

    async fn run(&mut self) -> Result<(), WorkerError> {
        for task in self.role.tasks() {
            let target = self.ctx.target_registry.get_target(task.target_id());
            if target.is_all() {
                self.queue_all(task.goal()).await?;
                break;
            }
            let _ = self.ctx.task_queue.queue(*task)?;
        }
        if self.role.is_main_worker() {
            self.ctx.task_results.mark_as_ready();
        }

        while self.ctx.coordinator.should_run() {
            // NOTE(@ostera): we don't want things to burn CPU cycles
            tokio::time::sleep(std::time::Duration::from_micros(10)).await;
            if let Err(err) = self.poll().await {
                self.ctx.coordinator.signal_shutdown();
                return Err(WorkerError::LocalWorkerError(err));
            }

            if self.role.is_main_worker() && self.ctx.task_results.has_all_expected_targets() {
                self.ctx.coordinator.signal_shutdown();
                break;
            }
        }
        Ok(())
    }
}

impl<R, P, E, S> LocalWorker<R, P, E, S>
where
    R: Resolver,
    P: Planner,
    E: Executor,
    S: Store,
{
    #[tracing::instrument(name = "LocalWorker::pool", skip(self))]
    pub async fn poll(&mut self) -> Result<(), LocalWorkerError> {
        let task = match self.ctx.task_queue.next() {
            Some(task) => task,
            None => return Ok(()),
        };

        debug!("Handling task {:#?}", task);

        match self.handle_task(task).await? {
            WorkerFlow::Complete {
                task,
                executable_spec,
                artifact_manifest,
            } => {
                debug!("Completed");
                self.ctx
                    .task_results
                    .add_task_result(task, executable_spec, artifact_manifest);
                self.ctx.task_queue.ack(task);
            }
            WorkerFlow::SwapWithSignatures { signature_ids } => {
                debug!("SwapWithSignatures: {}", signature_ids.len());
                let first_sig_id = signature_ids.first().unwrap();
                let first_task = UnregisteredTask::builder()
                    .goal(task.goal())
                    .target_id(task.target_id())
                    .signature_id(*first_sig_id)
                    .build()
                    .unwrap();

                let first_task = self.ctx.task_registry.register(first_task);

                self.ctx.task_queue.swap(task, first_task);

                for sig_id in signature_ids.iter().skip(1) {
                    let sig = self.ctx.signature_registry.get(*sig_id);
                    let task = UnregisteredTask::builder()
                        .goal(Goal::from_rule_name(sig.rule()))
                        .target_id(task.target_id())
                        .signature_id(*sig_id)
                        .build()
                        .unwrap();
                    let task = self.ctx.task_registry.register(task);
                    let _ = self.ctx.task_queue.queue(task)?;
                }
            }

            WorkerFlow::Skipped(task) => {
                debug!("Skipped task {:#?}", task);
                self.ctx.task_queue.skip(task);
            }
            WorkerFlow::QueueDeps { deps } => {
                debug!("QueueDeps: {} deps: {:#?}", deps.len(), &deps);
                self.ctx.task_queue.queue_deps(task, &deps)?;
                self.ctx.task_queue.nack(task);
            }
        }

        Ok(())
    }

    pub async fn handle_task(&mut self, task: Task) -> Result<WorkerFlow, LocalWorkerError> {
        let goal = task.goal();
        let target = self.ctx.target_registry.get_target(task.target_id());

        self.ctx
            .event_channel
            .send(WorkerEvent::TargetBuildStarted {
                target: (*target).clone(),
                goal,
            });

        if task.signature_id().is_none() {
            return Ok(
                match self.ctx.resolver.resolve(task, target.clone()).await? {
                    ResolutionFlow::MissingDeps { deps } => WorkerFlow::QueueDeps { deps },
                    ResolutionFlow::Resolved { signature_ids } => {
                        WorkerFlow::SwapWithSignatures { signature_ids }
                    }
                    _ => WorkerFlow::Skipped(task),
                },
            );
        }

        let signature = self
            .ctx
            .signature_registry
            .get(task.signature_id().unwrap());

        let executable_spec = match self
            .planner
            .plan(task, &signature, self.env.clone())
            .await?
        {
            PlanningFlow::Planned { spec } => spec,
            PlanningFlow::MissingDeps { deps } => return Ok(WorkerFlow::QueueDeps { deps }),
            _ => return Ok(WorkerFlow::Skipped(task)),
        };

        for dep in executable_spec.deps().runtime_deps() {
            self.ctx.task_queue.queue(*dep)?;
        }

        let (artifact_manifest, cache_status) =
            match self.executor.execute(&executable_spec).await? {
                ExecutionFlow::Completed(manifest, status) => (manifest, status),
                ExecutionFlow::MissingDeps { deps } => return Ok(WorkerFlow::QueueDeps { deps }),
                flow => {
                    dbg!(flow);
                    return Ok(WorkerFlow::Skipped(task));
                }
            };

        self.ctx
            .event_channel
            .send(WorkerEvent::TargetBuildCompleted {
                target: (*target).clone(),
                goal,
                cache_status,
                signature: signature.name().to_string(),
            });

        Ok(WorkerFlow::Complete {
            task,
            executable_spec,
            artifact_manifest,
        })
    }

    async fn queue_all(&self, goal: Goal) -> Result<(), WorkerError> {
        let root = self
            .ctx
            .workspace_manager
            .current_workspace()
            .root()
            .to_path_buf();

        let skip_patterns = {
            let mut builder = globset::GlobSetBuilder::new();
            for pattern in &["*/.warp*", "*warp-outputs*", "*.git*", "*target/*"] {
                let glob = globset::Glob::new(pattern).unwrap();
                builder.add(glob);
            }
            builder.build().unwrap()
        };

        let mut dirs = vec![root.clone()];
        while let Some(dir) = dirs.pop() {
            let mut read_dir = fs::read_dir(&dir).await.unwrap();

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
                let target: Target = path.into();
                let target_id = self.ctx.target_registry.register_target(target);

                let unreg_task = UnregisteredTask::builder()
                    .target_id(target_id)
                    .goal(goal)
                    .build()
                    .unwrap();

                let task = self.ctx.task_registry.register(unreg_task);

                self.ctx.task_queue.queue(task).unwrap();
            }
        }

        Ok(())
    }
}

#[derive(Error, Debug)]
pub enum LocalWorkerError {
    #[error(transparent)]
    ResolverError(ResolverError),

    #[error(transparent)]
    PlannerError(PlannerError),

    #[error(transparent)]
    ExecutorError(ExecutorError),

    #[error(transparent)]
    TaskQueueError(TaskQueueError),
}

impl From<ExecutorError> for LocalWorkerError {
    fn from(value: ExecutorError) -> Self {
        LocalWorkerError::ExecutorError(value)
    }
}

impl From<ResolverError> for LocalWorkerError {
    fn from(err: ResolverError) -> Self {
        Self::ResolverError(err)
    }
}

impl From<PlannerError> for LocalWorkerError {
    fn from(value: PlannerError) -> Self {
        LocalWorkerError::PlannerError(value)
    }
}

impl From<TaskQueueError> for LocalWorkerError {
    fn from(err: TaskQueueError) -> Self {
        LocalWorkerError::TaskQueueError(err)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::code::CodeManager;

    use crate::model::{ConcreteTarget, ExecutableSpec, Goal, Signature, Target};
    use crate::planner::PlannerError;
    use crate::resolver::{SignatureRegistry, TargetRegistry};
    use crate::store::{ArtifactManifest, Store, StoreError};
    use crate::sync::*;
    use crate::testing::TestMatcherRegistry;
    use crate::worker::{Role, Task, TaskRegistry, TaskResults};
    use crate::workspace::WorkspaceManager;
    use crate::Config;
    use async_trait::async_trait;
    use quickcheck::Arbitrary;
    use std::path::PathBuf;
    use url::Url;

    #[derive(Debug, Clone)]
    struct NoopExecutor;
    #[async_trait]
    impl Executor for NoopExecutor {
        type Context = ();

        fn new(_ctx: Self::Context) -> Result<Self, ExecutorError> {
            Ok(Self)
        }

        async fn execute(
            &mut self,
            _spec: &ExecutableSpec,
        ) -> Result<ExecutionFlow, ExecutorError> {
            todo!()
        }
    }

    #[derive(Debug, Clone)]
    struct NoopStore;
    #[async_trait]
    impl Store for NoopStore {
        async fn install_from_manifest_url(
            &self,
            _url: &Url,
        ) -> Result<ArtifactManifest, StoreError> {
            Err(StoreError::Unknown)
        }

        async fn find(
            &self,
            _spec: &ExecutableSpec,
        ) -> Result<Option<ArtifactManifest>, StoreError> {
            Err(StoreError::Unknown)
        }

        async fn clean(&self, _spec: &ExecutableSpec) -> Result<(), StoreError> {
            Err(StoreError::Unknown)
        }

        async fn promote(&self, _am: &ArtifactManifest) -> Result<(), StoreError> {
            Err(StoreError::Unknown)
        }

        async fn save(
            &self,
            _spec: &ExecutableSpec,
            _manifest: &ArtifactManifest,
        ) -> Result<(), StoreError> {
            Err(StoreError::Unknown)
        }

        fn get_local_store_path_for_spec(&self, _spec: &ExecutableSpec) -> PathBuf {
            PathBuf::from("")
        }

        fn get_local_store_path_for_manifest(&self, _am: &ArtifactManifest) -> PathBuf {
            PathBuf::from("")
        }

        fn canonicalize_provided_artifact<N: AsRef<str>>(
            &self,
            _am: &ArtifactManifest,
            _name: N,
        ) -> Option<PathBuf> {
            None
        }
    }

    #[derive(Clone)]
    struct NoopPlanner;
    #[derive(Debug, Clone)]
    struct NoopContext;

    impl<R, S> From<LocalSharedContext<R, S>> for NoopContext
    where
        R: Resolver,
        S: Store,
    {
        fn from(_value: LocalSharedContext<R, S>) -> Self {
            Self
        }
    }

    #[async_trait(?Send)]
    impl Planner for NoopPlanner {
        type Context = NoopContext;

        fn new(_ctx: Self::Context) -> Result<Self, PlannerError> {
            Ok(Self)
        }

        async fn plan(
            &mut self,
            _task: Task,
            _sig: &Signature,
            _env: ExecutionEnvironment,
        ) -> Result<PlanningFlow, PlannerError> {
            Ok(PlanningFlow::MissingDeps { deps: vec![] })
        }
    }

    #[derive(Clone)]
    struct NoopResolver;

    #[async_trait]
    impl Resolver for NoopResolver {
        async fn resolve(
            &self,
            _task: Task,
            _target: Arc<Target>,
        ) -> Result<ResolutionFlow, ResolverError> {
            Ok(ResolutionFlow::IncompatibleTarget)
        }
    }

    #[tokio::test]
    async fn when_coordinator_marks_shutdown_the_worker_stops() {
        let warp_root = assert_fs::TempDir::new().unwrap();
        let config = Config::builder()
            .warp_root(warp_root.path().to_path_buf())
            .build()
            .unwrap();
        let task_registry = Arc::new(TaskRegistry::new());
        let target_registry = Arc::new(TargetRegistry::new());
        let signature_registry = Arc::new(SignatureRegistry::new());
        let test_matcher_registry = Arc::new(TestMatcherRegistry::new());
        let workspace_manager = WorkspaceManager::new(config.clone()).into();
        let task_results = Arc::new(TaskResults::new(
            task_registry.clone(),
            target_registry.clone(),
            signature_registry.clone(),
        ));
        let code_manager = Arc::new(
            CodeManager::new(
                config.clone(),
                NoopStore.into(),
                test_matcher_registry.clone(),
                target_registry.clone(),
                task_registry.clone(),
                task_results.clone(),
            )
            .unwrap(),
        );
        let ctx = LocalSharedContext::new(
            config,
            task_registry,
            target_registry,
            signature_registry,
            test_matcher_registry,
            NoopResolver,
            NoopStore.into(),
            workspace_manager,
            task_results,
            code_manager,
        );

        ctx.coordinator.signal_shutdown();

        let mut w: LocalWorker<NoopResolver, NoopPlanner, NoopExecutor, NoopStore> =
            LocalWorker::new(Role::MainWorker(vec![]), ctx).unwrap();
        w.run().await.unwrap();
    }

    #[tokio::test]
    async fn on_resolver_error_worker_signal_shutdown() {
        #[derive(Clone)]
        struct ErrResolver;
        #[async_trait]
        impl Resolver for ErrResolver {
            async fn resolve(
                &self,
                _task: Task,
                _target: Arc<Target>,
            ) -> Result<ResolutionFlow, ResolverError> {
                Err(ResolverError::Unknown("test error".to_string()))
            }
        }

        let warp_root = assert_fs::TempDir::new().unwrap();
        let config = Config::builder()
            .warp_root(warp_root.path().to_path_buf())
            .build()
            .unwrap();
        let task_registry = Arc::new(TaskRegistry::new());
        let target_registry = Arc::new(TargetRegistry::new());
        let signature_registry = Arc::new(SignatureRegistry::new());
        let test_matcher_registry = Arc::new(TestMatcherRegistry::new());
        let workspace_manager = WorkspaceManager::new(config.clone()).into();
        let task_results = Arc::new(TaskResults::new(
            task_registry.clone(),
            target_registry.clone(),
            signature_registry.clone(),
        ));
        let code_manager = Arc::new(
            CodeManager::new(
                config.clone(),
                NoopStore.into(),
                test_matcher_registry.clone(),
                target_registry.clone(),
                task_registry.clone(),
                task_results.clone(),
            )
            .unwrap(),
        );
        let ctx = LocalSharedContext::new(
            config,
            task_registry.clone(),
            target_registry,
            signature_registry,
            test_matcher_registry,
            ErrResolver,
            NoopStore.into(),
            workspace_manager,
            task_results,
            code_manager,
        );

        let mut gen = quickcheck::Gen::new(100);
        let target: Target = Arbitrary::arbitrary(&mut gen);
        let goal: Goal = Arbitrary::arbitrary(&mut gen);
        let target_id = ctx.target_registry.register_target(target);
        let unreg_task = UnregisteredTask::builder()
            .goal(goal)
            .target_id(target_id)
            .build()
            .unwrap();
        let task = task_registry.register(unreg_task);
        ctx.task_queue.queue(task).unwrap();
        assert!(!ctx.task_queue.is_empty());

        let mut w: LocalWorker<ErrResolver, NoopPlanner, NoopExecutor, NoopStore> =
            LocalWorker::new(Role::MainWorker(vec![]), ctx.clone()).unwrap();
        let err = w.run().await.unwrap_err();

        assert_matches!(
            err,
            WorkerError::LocalWorkerError(LocalWorkerError::ResolverError(ResolverError::Unknown(
                _
            )))
        );
        assert!(ctx.coordinator.should_shutdown());
    }

    #[tokio::test]
    async fn on_planner_error_worker_signal_shutdown() {
        #[derive(Clone)]
        struct ErrPlanner;
        #[async_trait(?Send)]
        impl Planner for ErrPlanner {
            type Context = ();

            fn new(_ctx: Self::Context) -> Result<Self, PlannerError> {
                Ok(Self)
            }

            async fn plan(
                &mut self,
                _task: Task,
                _sig: &Signature,
                _env: ExecutionEnvironment,
            ) -> Result<PlanningFlow, PlannerError> {
                Err(PlannerError::Unknown)
            }
        }

        #[derive(Clone)]
        struct DummyResolver {
            signature_registry: Arc<SignatureRegistry>,
        }

        #[async_trait]
        impl Resolver for DummyResolver {
            async fn resolve(
                &self,
                task: Task,
                target: Arc<Target>,
            ) -> Result<ResolutionFlow, ResolverError> {
                let target = ConcreteTarget::new(
                    task.goal(),
                    task.target_id(),
                    target,
                    "".into(),
                    "".into(),
                );
                let signature_ids = vec![self.signature_registry.register(
                    Signature::builder()
                        .name("test_signature")
                        .rule("dummy_rule")
                        .target(target)
                        .build()
                        .unwrap(),
                )];
                Ok(ResolutionFlow::Resolved { signature_ids })
            }
        }

        let warp_root = assert_fs::TempDir::new().unwrap();
        let config = Config::builder()
            .warp_root(warp_root.path().to_path_buf())
            .build()
            .unwrap();
        let task_registry = Arc::new(TaskRegistry::new());
        let target_registry = Arc::new(TargetRegistry::new());
        let signature_registry = Arc::new(SignatureRegistry::new());
        let test_matcher_registry = Arc::new(TestMatcherRegistry::new());
        let workspace_manager = WorkspaceManager::new(config.clone()).into();
        let task_results = Arc::new(TaskResults::new(
            task_registry.clone(),
            target_registry.clone(),
            signature_registry.clone(),
        ));
        let code_manager = Arc::new(
            CodeManager::new(
                config.clone(),
                NoopStore.into(),
                test_matcher_registry.clone(),
                target_registry.clone(),
                task_registry.clone(),
                task_results.clone(),
            )
            .unwrap(),
        );
        let ctx = LocalSharedContext::new(
            config,
            task_registry.clone(),
            target_registry,
            signature_registry.clone(),
            test_matcher_registry,
            DummyResolver { signature_registry },
            NoopStore.into(),
            workspace_manager,
            task_results,
            code_manager,
        );

        let mut gen = quickcheck::Gen::new(100);
        let target: Target = Arbitrary::arbitrary(&mut gen);
        let goal: Goal = Arbitrary::arbitrary(&mut gen);
        let target_id = ctx.target_registry.register_target(target);
        let unreg_task = UnregisteredTask::builder()
            .goal(goal)
            .target_id(target_id)
            .build()
            .unwrap();
        let task = task_registry.register(unreg_task);
        ctx.task_queue.queue(task).unwrap();
        assert!(!ctx.task_queue.is_empty());

        let mut w: LocalWorker<DummyResolver, ErrPlanner, NoopExecutor, NoopStore> =
            LocalWorker::new(Role::MainWorker(vec![]), ctx.clone()).unwrap();
        let err = w.run().await.unwrap_err();

        assert_matches!(
            err,
            WorkerError::LocalWorkerError(LocalWorkerError::PlannerError(PlannerError::Unknown))
        );
        assert!(ctx.coordinator.should_shutdown());
    }
}
