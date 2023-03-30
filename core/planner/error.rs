use crate::code::CodeManagerError;
use crate::model::{DependenciesError, ExecutableSpecError};
use crate::rules::RuleExecutorError;
use crate::tricorder::TricorderManagerError;
use thiserror::*;

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

    #[error(transparent)]
    CodeManagerError(CodeManagerError),

    #[error(transparent)]
    TricorderManagerError(TricorderManagerError),
}

impl From<CodeManagerError> for PlannerError {
    fn from(value: CodeManagerError) -> Self {
        PlannerError::CodeManagerError(value)
    }
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

impl From<TricorderManagerError> for PlannerError {
    fn from(value: TricorderManagerError) -> Self {
        PlannerError::TricorderManagerError(value)
    }
}
