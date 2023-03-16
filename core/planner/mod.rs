//! # Flexible Task Planning Engine

mod default;
mod default_planner_context;

use async_trait::async_trait;
pub use default::*;
pub use default_planner_context::*;

use crate::model::{
    Dependencies, DependenciesError, ExecutableSpec, ExecutableSpecError, ExecutionEnvironment,
    Goal, Signature, TargetId,
};
use crate::rules::RuleExecutorError;
use thiserror::*;

#[derive(Debug)]
pub enum PlanningFlow {
    Planned { spec: ExecutableSpec },
    MissingDeps { deps: Vec<TargetId> },
    FoundAllDeps { deps: Dependencies },
}

#[async_trait(?Send)]
pub trait Planner {
    type Context: Sync + Send + Clone + Sized;

    fn new(ctx: Self::Context) -> Result<Self, PlannerError>
    where
        Self: Sized;

    async fn plan(
        &mut self,
        goal: Goal,
        sig: Signature,
        env: ExecutionEnvironment,
    ) -> Result<PlanningFlow, PlannerError>;
}

#[derive(Error, Debug)]
pub enum PlannerError {
    #[error("Something went wrong with this planner")]
    Unknown,

    #[error(transparent)]
    RuleExecutorError(RuleExecutorError),

    #[error(transparent)]
    ExecutableSpecError(ExecutableSpecError),

    #[error(transparent)]
    DependenciesError(DependenciesError),
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

impl From<DependenciesError> for PlannerError {
    fn from(value: DependenciesError) -> Self {
        PlannerError::DependenciesError(value)
    }
}
