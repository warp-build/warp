use std::marker::PhantomData;

use crate::sync::Arc;

use super::*;
use crate::events::Event;
use crate::resolver::TargetId;
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
    ctx: SharedContext,
    _worker: PhantomData<W>,
}

impl<W: Worker> WorkerPool<W> {
    #[tracing::instrument(name = "WorkerPool::from_shared_context", skip(ctx))]
    pub fn from_shared_context(ctx: SharedContext) -> Self {
        let worker_pool = LocalPoolHandle::new({
            // NOTE(@ostera): we want to make sure you don't ask for more workers than the number
            // of CPUs available.
            let max = num_cpus::get();
            // magic + 1 here means "however many workers you want + the main worker"
            let ask = ctx.options.max_local_workers() + 1;
            ask.min(max)
        });

        Self {
            worker_pool,
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
            self.ctx
                .event_channel
                .send(Event::BuildCompleted(std::time::Instant::now()));
            let empty_results = Arc::new(TaskResults::default());
            return Ok(empty_results);
        }

        let main_worker = self.spawn_worker(Role::MainWorker);

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

        Ok(self.ctx.task_results.clone())
    }

    fn spawn_worker(&self, role: Role) -> tokio::task::JoinHandle<Result<(), WorkerPoolError>> {
        let ctx = self.ctx.clone();
        self.worker_pool.spawn_pinned(move || async move {
            let mut worker = W::new(role, ctx)?;
            worker.setup_and_run().await?;
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
    use crate::resolver::Target;
    use crate::Config;
    use assert_fs::prelude::*;

    #[derive(Debug)]
    struct NoopWorker;

    #[async_trait]
    impl Worker for NoopWorker {
        fn new(_role: Role, _ctx: SharedContext) -> Result<Self, WorkerError> {
            Ok(NoopWorker)
        }
        async fn setup_and_run(&mut self) -> Result<(), WorkerError> {
            loop {
                tokio::time::sleep(std::time::Duration::from_millis(1)).await;
            }
            Ok(())
        }
    }

    fn new_pool<W: Worker>() -> WorkerPool<W> {
        let ec = Arc::new(EventChannel::new());
        let config = Config::default();
        let ctx = SharedContext::new(ec, config);
        WorkerPool::from_shared_context(ctx)
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

        // Configure Warp to use our test workspace as the invocation dir.
        // In practice this may not hold, but in the test its easier to just go to the workspace
        // than to go somewhere _within it_.
        let ec = Arc::new(EventChannel::new());
        let config = Config::builder()
            .invocation_dir(root.path().into())
            .max_local_workers(0)
            .build()
            .unwrap();
        let ctx = SharedContext::new(ec, config);

        // Create a file that we will use as a target.
        let input_file = root.child("foo.txt");
        input_file.touch().unwrap();

        // Make our new file our target
        let target: Target = input_file.path().into();
        let target_id = ctx.target_registry.register_target(target);

        /*
        #[derive(Debug)]
        struct FixtureWorker {
            ctx: SharedContext,
        }

        #[async_trait]
        impl Worker for FixtureWorker {
            fn new(_role: Role, ctx: SharedContext) -> Result<Self, WorkerError> {
                Ok(FixtureWorker(ctx))
            }

            async fn setup_and_run(&mut self) -> Result<(), LocalWorkerError> {
                loop {
                    tokio::time::sleep(std::time::Duration::from_millis(1)).await;
                    self.ctx.task_results.add_target_manifest(
                        target,
                        executable_target,
                        target_manifest,
                    )
                }
                Ok(())
            }
        }
        */

        // let pool: WorkerPool<FixtureWorker> = WorkerPool::from_shared_context(ctx);
        //  let results = pool.execute(&[target_id]).await.unwrap();
        // assert!(results.len() == 0);
        assert!(false);
    }
}
