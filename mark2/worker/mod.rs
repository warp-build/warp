//! # Location-transparent, parallel worker pools

mod coordinator;
mod env;
mod local_shared_context;
mod local_worker;
mod pool;
mod task;
mod task_queue;
mod task_results;

use coordinator::*;
use env::*;
pub use local_shared_context::*;
pub use local_worker::*;
pub use pool::*;
pub use task::*;
use task_queue::*;
pub use task_results::*;

use crate::model::TargetId;
use crate::sync::*;
use async_trait::async_trait;
use std::fmt::Debug;
use thiserror::*;

#[derive(Error, Debug)]
pub enum WorkerError {
    #[error(transparent)]
    LocalWorkerError(LocalWorkerError),

    #[error(transparent)]
    TaskQueueError(TaskQueueError),
}

pub trait Context: Sync + Send + Clone + Sized {
    fn results(&self) -> Arc<TaskResults>;
}

#[async_trait]
pub trait Worker: Sized {
    type Context: Context;
    fn new(role: Role, ctx: Self::Context) -> Result<Self, WorkerError>;
    async fn run(&mut self) -> Result<(), WorkerError>;
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
