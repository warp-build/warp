use std::sync::Arc;

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
#[derive(Clone, Debug)]
pub struct WorkerPool {
    worker_pool: LocalPoolHandle,
    ctx: SharedContext,
}

impl WorkerPool {
    #[tracing::instrument(name = "WorkerPool::from_shared_context", skip(ctx))]
    pub fn from_shared_context(ctx: SharedContext) -> Self {
        let worker_pool = LocalPoolHandle::new({
            let max = num_cpus::get();
            let curr = ctx.options.max_local_workers() + 2;
            curr.min(max)
        });

        Self { worker_pool, ctx }
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
            let mut worker = LocalWorker::new(role, ctx).map_err(WorkerPoolError::WorkerError)?;

            worker
                .setup_and_run()
                .await
                .map_err(WorkerPoolError::WorkerError)
        })
    }
}

#[derive(Error, Debug)]
pub enum WorkerPoolError {
    #[error(transparent)]
    WorkerError(LocalWorkerError),
}

#[cfg(test)]
mod tests {}
