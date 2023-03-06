//! # Flexible Task Planning Engine

mod default;
mod default_planner_context;

pub use default::*;
pub use default_planner_context::*;

use crate::model::{Dependencies, ExecutableSpec, ExecutableSpecError, Requirement, Signature};
use crate::rules::RuleExecutorError;
use crate::worker::ExecutionEnvironment;
use futures::Future;
use std::pin::Pin;
use thiserror::*;

#[derive(Debug)]
pub enum PlanningFlow {
    Planned { spec: ExecutableSpec },
    MissingDeps { requirements: Vec<Requirement> },
    FoundAllDeps { deps: Dependencies },
}

/// NOTE(@ostera): because the Planner uses a RuleExecutor that uses Deno in one of its implementations, we can't use the #[async_trait] macro. This macro automatically marks all futures as +Send, which forces
/// Send on this RuleExecutor, which forces Send on the Deno instances.
///
pub trait Planner {
    type Context: Sync + Send + Clone + Sized;

    fn new(ctx: Self::Context) -> Result<Self, PlannerError>
    where
        Self: Sized;

    fn plan<'a>(
        &'a mut self,
        sig: Signature,
        env: ExecutionEnvironment,
    ) -> Pin<Box<dyn Future<Output = Result<PlanningFlow, PlannerError>> + 'a>>;
}

#[derive(Error, Debug)]
pub enum PlannerError {
    #[error("Something went wrong with this planner")]
    Unknown,

    #[error(transparent)]
    RuleExecutorError(RuleExecutorError),

    #[error(transparent)]
    ExecutableSpecError(ExecutableSpecError),
}

impl From<RuleExecutorError> for PlannerError {
    fn from(value: RuleExecutorError) -> Self {
        PlannerError::RuleExecutorError(value)
    }
}

impl From<ExecutableSpecError> for PlannerError {
    fn from(value: ExecutableSpecError) -> Self {
        PlannerError::ExecutableSpecError(value)
    }
}
