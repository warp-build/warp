//! # Location-transparent, parallel worker pools

mod coordinator;
mod env;

mod local_worker;
mod pool;
mod shared_context;
mod task;
mod task_queue;
mod task_results;

use coordinator::*;
use env::*;
use local_worker::*;
pub use pool::*;
pub use shared_context::*;
use task::*;
use task_queue::*;
pub use task_results::*;

use thiserror::*;

#[derive(Error, Debug)]
pub enum WorkerError {
    #[error(transparent)]
    LocalWorkerError(LocalWorkerError),
}

/// The role that a given worker has. The MainWorker is in charge of terminating execution
/// normally, whereas the HelperWorker(id) are there to speed things up if more cores are
/// available.
///
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Role {
    MainWorker,
    HelperWorker(usize),
}

impl Role {
    pub fn is_main_worker(&self) -> bool {
        matches!(&self, Role::MainWorker)
    }
}
