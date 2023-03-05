use super::*;
use crate::events::{Event, EventChannel};
use crate::model::TargetId;
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
    pub async fn execute(&self, targets: &[TargetId]) -> Result<Arc<TaskResults>, WorkerPoolError> {
        if targets.is_empty() {
            self.event_channel
                .send(Event::BuildCompleted(std::time::Instant::now()));
            let empty_results = Arc::new(TaskResults::default());
            return Ok(empty_results);
        }

        let main_worker = self.spawn_worker(Role::MainWorker, Some(targets));

        // NOTE(@ostera): we are skipping the 1st threads since that's the main worker.
        let mut worker_tasks = vec![];
        for worker_id in 1..self.worker_pool.num_threads() {
            worker_tasks.push(self.spawn_worker(Role::HelperWorker(worker_id), None));
        }

        let (main_result, helper_results) =
            futures::future::join(main_worker, futures::future::join_all(worker_tasks)).await;

        for task_result in helper_results {
            task_result.unwrap()?;
        }
        main_result.unwrap()?;

        self.event_channel
            .send(Event::BuildCompleted(std::time::Instant::now()));

        Ok(self.ctx.results().clone())
    }

    fn spawn_worker(
        &self,
        role: Role,
        _targets: Option<&[TargetId]>,
    ) -> tokio::task::JoinHandle<Result<(), WorkerPoolError>> {
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
    use super::*;
    use crate::events::EventChannel;
    use crate::model::{Goal, Target};
    use crate::resolver::{ResolutionFlow, Resolver, ResolverError, TargetRegistry};
    use crate::Config;
    use assert_fs::prelude::*;

    #[derive(Clone)]
    struct NoopResolver;

    #[async_trait]
    impl Resolver for NoopResolver {
        async fn resolve(
            &self,
            _goal: Goal,
            _target: Arc<Target>,
        ) -> Result<ResolutionFlow, ResolverError> {
            Ok(ResolutionFlow::MissingDependencies)
        }
    }

    #[derive(Debug)]
    struct NoopWorker;

    #[async_trait]
    impl Worker for NoopWorker {
        type Context = LocalSharedContext<NoopResolver>;
        fn new(_role: Role, _ctx: Self::Context) -> Result<Self, WorkerError> {
            Ok(NoopWorker)
        }
        async fn run(&mut self) -> Result<(), WorkerError> {
            Ok(())
        }
    }

    fn new_pool<W>() -> WorkerPool<W>
    where
        W: Worker<Context = LocalSharedContext<NoopResolver>>,
    {
        let ec = Arc::new(EventChannel::new());
        let config = Config::default();
        let ctx = LocalSharedContext::new(ec.clone(), config.clone(), NoopResolver);
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
        let target_id = target_registry.register_target(target);

        #[derive(Debug, Clone)]
        struct FixtureContext {
            target_id: TargetId,
            task_results: Arc<TaskResults>,
        }
        impl Context for FixtureContext {
            fn results(&self) -> Arc<TaskResults> {
                self.task_results.clone()
            }
        }
        let ctx = FixtureContext {
            target_id,
            task_results: Arc::new(TaskResults::new(target_registry.clone())),
        };

        #[derive(Debug)]
        struct FixtureWorker {
            ctx: FixtureContext,
        }

        #[async_trait]
        impl Worker for FixtureWorker {
            type Context = FixtureContext;
            fn new(_role: Role, ctx: Self::Context) -> Result<Self, WorkerError> {
                Ok(FixtureWorker { ctx })
            }

            async fn run(&mut self) -> Result<(), WorkerError> {
                self.ctx
                    .task_results
                    .add_target_manifest(self.ctx.target_id, (), ());
                Ok(())
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
        let results = pool.execute(&[target_id]).await.unwrap();
        assert_eq!(results.len(), 1);
    }
}
