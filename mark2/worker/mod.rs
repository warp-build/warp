//! # Location-transparent, parallel worker pools

mod coordinator;
mod local_shared_context;
mod local_worker;
mod pool;
mod task;
mod task_queue;
mod task_results;

use coordinator::*;
use futures::Future;
pub use local_shared_context::*;
pub use local_worker::*;
pub use pool::*;
pub use task::*;
use task_queue::*;
pub use task_results::*;

use crate::planner::PlannerError;
use crate::sync::*;
use std::fmt::Debug;
use std::pin::Pin;
use thiserror::*;

#[derive(Error, Debug)]
pub enum WorkerError {
    #[error(transparent)]
    LocalWorkerError(LocalWorkerError),

    #[error(transparent)]
    TaskQueueError(TaskQueueError),

    #[error(transparent)]
    PlannerError(PlannerError),
}

/// A Context object used across workers. It must be thread-safe and shareable, but the things it
/// contains do not need to be.
///
/// The only requirement is a way to extract the results of the task execution.
///
pub trait Context: Sync + Send + Clone + Sized {
    fn results(&self) -> Arc<TaskResults>;
}

/// A worker in the Worker Pool.
///
/// This trait is used to build specific workers that are usable within the WorkerPool. It requires
/// you to specify a type of context that implements the [Context] trait.
///
/// Normally workers have a set up that happens during the `run` phase, so their construction is
/// sync.
///
/// NOTE(@ostera): because the LocalWorker uses a Planner, which is not Send nor Sync, we can't
/// use the #[async_trait] because it automatically marks all futures as +Send, which forces
/// Send on this Worker, which forces Send on the inner Planner, which can't be Send because of
/// Deno.
///
pub trait Worker {
    type Context: Context;

    fn new(role: Role, ctx: Self::Context) -> Result<Self, WorkerError>
    where
        Self: Sized;

    fn run<'a>(&'a mut self) -> Pin<Box<dyn Future<Output = Result<(), WorkerError>> + 'a>>;
}

/// The role that a given worker has. The MainWorker is in charge of terminating execution
/// normally, whereas the HelperWorker(id) are there to speed things up if more cores are
/// available.
///
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Role {
    MainWorker(Vec<Task>),
    HelperWorker(usize),
}

impl Role {
    pub fn is_main_worker(&self) -> bool {
        matches!(&self, Role::MainWorker(_))
    }

    pub fn tasks(&self) -> &[Task] {
        match self {
            Role::MainWorker(ts) => ts,
            Role::HelperWorker(_) => &[],
        }
    }
}
