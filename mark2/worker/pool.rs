use super::*;
use crate::events::{Event, EventChannel};
use crate::sync::Arc;
use crate::Config;
use std::marker::PhantomData;
use thiserror::*;
use tokio_util::task::LocalPoolHandle;
use tracing::*;

/// The WorkerPool spins up a pool of workers that can execute work.
///
/// This pool is _lazy_ and will only spin up workers at execution time. However, at that time it
/// is _eager_, and will spin up as many workers as allowed in `Config.concurrency_limit`.
///
#[derive(Debug)]
pub struct WorkerPool<W: Worker> {
    worker_pool: LocalPoolHandle,
    event_channel: Arc<EventChannel>,
    ctx: W::Context,
    _worker: PhantomData<W>,
}

impl<Ctx: Context + 'static, W: Worker<Context = Ctx>> WorkerPool<W> {
    #[tracing::instrument(name = "WorkerPool::from_shared_context", skip(ctx))]
    pub fn from_shared_context(event_channel: Arc<EventChannel>, cfg: Config, ctx: Ctx) -> Self {
        let worker_pool = LocalPoolHandle::new({
            // NOTE(@ostera): we want to make sure you don't ask for more workers than the number
            // of CPUs available.
            let max = num_cpus::get();
            // magic + 1 here means "however many workers you want + the main worker"
            let ask = cfg.max_local_workers() + 1;
            ask.min(max)
        });

        Self {
            worker_pool,
            event_channel,
            ctx,
            _worker: PhantomData::default(),
        }
    }

    /// Execute a number of `Target`s. The order of execution is only guaranteed to follow some
    /// topographical-sort over the dependency graph of the targets.
    ///
    #[tracing::instrument(name = "WorkerPool::execute", skip(self))]
    pub async fn execute(&self, tasks: &[Task]) -> Result<Arc<TaskResults>, WorkerPoolError> {
        if tasks.is_empty() {
            self.event_channel
                .send(Event::BuildCompleted(std::time::Instant::now()));
            let empty_results = Arc::new(TaskResults::default());
            return Ok(empty_results);
        }

        let main_worker = self.spawn_worker(Role::MainWorker(tasks.to_vec()));

        // NOTE(@ostera): we are skipping the 1st threads since that's the main worker.
        let mut worker_tasks = vec![];
        for worker_id in 1..self.worker_pool.num_threads() {
            worker_tasks.push(self.spawn_worker(Role::HelperWorker(worker_id)));
        }

        let (main_result, helper_results) =
            futures::future::join(main_worker, futures::future::join_all(worker_tasks)).await;

        for task_result in helper_results {
            task_result.unwrap()?;
        }
        main_result.unwrap()?;

        self.event_channel
            .send(Event::BuildCompleted(std::time::Instant::now()));

        Ok(self.ctx.results())
    }

    fn spawn_worker(&self, role: Role) -> tokio::task::JoinHandle<Result<(), WorkerPoolError>> {
        let ctx = self.ctx.clone();
        self.worker_pool.spawn_pinned(move || async move {
            let mut worker = W::new(role, ctx)?;
            worker.run().await?;
            Ok(())
        })
    }
}

#[derive(Error, Debug)]
pub enum WorkerPoolError {
    #[error(transparent)]
    WorkerError(WorkerError),
}

impl From<WorkerError> for WorkerPoolError {
    fn from(err: WorkerError) -> Self {
        Self::WorkerError(err)
    }
}

#[cfg(test)]
mod tests {
    use core::future::Future;
    use core::pin::Pin;
    use std::path::PathBuf;

    use super::*;
    use crate::events::EventChannel;
    use crate::model::{
        ConcreteTarget, ExecutableSpec, ExecutionEnvironment, Goal, Signature, Target, TargetId,
    };
    use crate::planner::{Planner, PlannerError, PlanningFlow};
    use crate::resolver::{ResolutionFlow, Resolver, ResolverError, TargetRegistry};
    use crate::store::{ArtifactManifest, ManifestUrl, Store, StoreError};
    use crate::worker::local::LocalSharedContext;
    use crate::workspace::WorkspaceManager;
    use crate::Config;
    use assert_fs::prelude::*;
    use async_trait::async_trait;
    use futures::FutureExt;

    #[derive(Debug, Clone)]
    struct NoopStore;
    #[async_trait]
    impl Store for NoopStore {
        async fn install_from_manifest_url(
            &self,
            _url: &ManifestUrl,
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
    struct NoopResolver;

    #[async_trait]
    impl Resolver for NoopResolver {
        async fn resolve(
            &self,
            _goal: Goal,
            _target_id: TargetId,
            _target: Arc<Target>,
        ) -> Result<ResolutionFlow, ResolverError> {
            Ok(ResolutionFlow::IncompatibleTarget)
        }
    }

    #[derive(Clone)]
    struct NoopPlanner;
    #[derive(Debug, Clone)]
    struct NoopContext;

    impl Planner for NoopPlanner {
        type Context = NoopContext;

        fn new(_ctx: Self::Context) -> Result<Self, PlannerError> {
            Ok(Self)
        }

        fn plan(
            &mut self,
            _sig: Signature,
            _env: ExecutionEnvironment,
        ) -> Pin<Box<dyn Future<Output = Result<PlanningFlow, PlannerError>>>> {
            async move { Ok(PlanningFlow::MissingDeps { deps: vec![] }) }.boxed_local()
        }
    }

    #[derive(Debug)]
    struct NoopWorker;

    impl Worker for NoopWorker {
        type Context = LocalSharedContext<NoopResolver, NoopStore>;
        fn new(_role: Role, _ctx: Self::Context) -> Result<Self, WorkerError> {
            Ok(NoopWorker)
        }
        fn run(&mut self) -> Pin<Box<dyn Future<Output = Result<(), WorkerError>>>> {
            async move { Ok(()) }.boxed_local()
        }
    }

    fn new_pool<W>() -> WorkerPool<W>
    where
        W: Worker<Context = LocalSharedContext<NoopResolver, NoopStore>>,
    {
        let ec = Arc::new(EventChannel::new());
        let config = Config::default();

        let workspace_manager = WorkspaceManager::new().into();
        let target_registry = Arc::new(TargetRegistry::new());
        let ctx = LocalSharedContext::new(
            ec.clone(),
            config.clone(),
            target_registry,
            NoopResolver,
            NoopStore.into(),
            workspace_manager,
        );
        WorkerPool::from_shared_context(ec, config, ctx)
    }

    #[tokio::test]
    async fn executing_no_targets_yields_empty_results() {
        let pool: WorkerPool<NoopWorker> = new_pool();
        let results = pool.execute(&[]).await.unwrap();
        assert!(results.len() == 0);
    }

    #[tokio::test]
    async fn executing_targets_yields_results() {
        // Create a temporary folder for our test workspace.
        let root = assert_fs::TempDir::new().unwrap();

        // Create a file that we will use as a target.
        let input_file = root.child("foo.txt");
        input_file.touch().unwrap();

        // Make our new file our target
        let target_registry = Arc::new(TargetRegistry::new());
        let target: Target = input_file.path().into();
        let target_id = target_registry.register_target(&target);

        #[derive(Debug, Clone)]
        struct FixtureContext {
            target_id: TargetId,
            target: ConcreteTarget,
            task_results: Arc<TaskResults>,
            env: ExecutionEnvironment,
        }
        impl Context for FixtureContext {
            fn results(&self) -> Arc<TaskResults> {
                self.task_results.clone()
            }
        }
        let ctx = FixtureContext {
            target_id,
            target: ConcreteTarget::new(Goal::Build, target_id, target.into(), "".into()),
            task_results: Arc::new(TaskResults::new(target_registry.clone())),
            env: Default::default(),
        };

        #[derive(Debug)]
        struct FixtureWorker {
            ctx: FixtureContext,
        }

        impl Worker for FixtureWorker {
            type Context = FixtureContext;
            fn new(_role: Role, ctx: Self::Context) -> Result<Self, WorkerError> {
                Ok(FixtureWorker { ctx })
            }

            fn run<'a>(
                &'a mut self,
            ) -> Pin<Box<dyn Future<Output = Result<(), WorkerError>> + 'a>> {
                async move {
                    let manifest = ArtifactManifest::default();
                    let spec = ExecutableSpec::builder()
                        .target(self.ctx.target.clone())
                        .signature(
                            Signature::builder()
                                .target(self.ctx.target.clone())
                                .rule("test_rule".into())
                                .build()
                                .unwrap(),
                        )
                        .exec_env(self.ctx.env.clone())
                        .hash_and_build(&*self.ctx.task_results)
                        .unwrap();

                    self.ctx
                        .task_results
                        .add_task_result(self.ctx.target_id, spec, manifest);
                    Ok(())
                }
                .boxed_local()
            }
        }

        let ec = Arc::new(EventChannel::new());
        // Configure Warp to use our test workspace as the invocation dir.
        // In practice this may not hold, but in the test its easier to just go to the workspace
        // than to go somewhere _within it_.
        let config = Config::builder()
            .invocation_dir(root.path().into())
            .max_local_workers(0)
            .build()
            .unwrap();
        let pool: WorkerPool<FixtureWorker> = WorkerPool::from_shared_context(ec, config, ctx);
        let results = pool
            .execute(&[Task::new(Goal::Build, target_id)])
            .await
            .unwrap();
        assert_eq!(results.len(), 1);
    }
}
