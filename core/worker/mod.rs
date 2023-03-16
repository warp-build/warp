//! # Location-transparent, parallel worker pools

mod coordinator;
mod error;
pub mod local;
mod pool;
mod task;
mod task_queue;
mod task_results;

pub use error::*;
pub use pool::*;
pub use task::*;
pub use task_results::*;

use crate::sync::*;
use async_trait::async_trait;
use std::fmt::Debug;

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
#[async_trait(?Send)]
pub trait Worker {
    type Context: Context;

    fn new(role: Role, ctx: Self::Context) -> Result<Self, WorkerError>
    where
        Self: Sized;

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
