use super::*;
use crate::events::Event;
use crate::resolver::TargetId;
use thiserror::*;
use tokio_util::task::LocalPoolHandle;
use tracing::*;

#[derive(Error, Debug)]
pub enum WorkerPoolError {
    #[error(transparent)]
    WorkerError(LocalWorkerError),
}

/// The WorkerPool spins up a pool of workers that can execute work.
///
#[derive(Clone, Debug)]
pub struct WorkerPool {
    worker_pool: LocalPoolHandle,
    ctx: SharedContext,
}

impl WorkerPool {
    #[tracing::instrument(name = "WorkerPool::from_shared_context", skip(ctx))]
    pub async fn from_shared_context(ctx: SharedContext) -> Result<Self, WorkerPoolError> {
        let worker_pool = LocalPoolHandle::new({
            let max = num_cpus::get();
            let curr = ctx.options.concurrency_limit + 2;
            curr.min(max)
        });

        Ok(Self {
            worker_pool,
            ctx: ctx.clone(),
        })
    }

    #[tracing::instrument(name = "WorkerPool::execute", skip(self))]
    pub async fn execute(&self, targets: &[TargetId]) -> Result<(), WorkerPoolError> {
        if targets.is_empty() {
            self.ctx
                .event_channel
                .send(Event::BuildCompleted(std::time::Instant::now()));
            return Ok(());
        }

        let main_worker = self.spawn_worker(Role::MainWorker);

        // NOTE(@ostera): we are leaving 2 threads for the main worker and the queuer
        let mut worker_tasks = vec![];
        for worker_id in 2..self.worker_pool.num_threads() {
            worker_tasks.push(self.spawn_worker(Role::HelperWorker(worker_id)));
        }

        let (main_result, helper_results) =
            futures::future::join(main_worker, futures::future::join_all(worker_tasks)).await;

        for task_result in helper_results {
            task_result.unwrap()?;
        }
        main_result.unwrap()?;

        Ok(())
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

#[cfg(test)]
mod tests {}
