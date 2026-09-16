use super::local::LocalWorkerError;
use super::task_queue::TaskQueueError;
use crate::executor::ExecutorError;
use crate::planner::PlannerError;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum WorkerError {
    #[error(transparent)]
    LocalWorkerError(LocalWorkerError),

    #[error(transparent)]
    TaskQueueError(TaskQueueError),

    #[error(transparent)]
    PlannerError(PlannerError),

    #[error(transparent)]
    ExecutorError(ExecutorError),
}

impl From<ExecutorError> for WorkerError {
    fn from(value: ExecutorError) -> Self {
        Self::ExecutorError(value)
    }
}

impl From<PlannerError> for WorkerError {
    fn from(value: PlannerError) -> Self {
        WorkerError::PlannerError(value)
    }
}

impl From<TaskQueueError> for WorkerError {
    fn from(err: TaskQueueError) -> Self {
        WorkerError::TaskQueueError(err)
    }
}

impl From<LocalWorkerError> for WorkerError {
    fn from(err: LocalWorkerError) -> Self {
        Self::LocalWorkerError(err)
    }
}
