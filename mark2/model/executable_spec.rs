use super::{ConcreteTarget, Dependencies};
use chrono::{DateTime, Utc};
use thiserror::Error;

#[derive(Builder, Debug)]
#[builder(build_fn(error = "ExecutableSpecError"))]
pub struct ExecutableSpec {
    planning_start_time: DateTime<Utc>,
    planning_end_time: DateTime<Utc>,
    target: ConcreteTarget,
    deps: Dependencies,
}

impl ExecutableSpec {
    pub fn builder() -> ExecutableSpecBuilder {
        Default::default()
    }

    /// When this target planning started.
    pub fn planning_start_time(&self) -> DateTime<Utc> {
        self.planning_start_time
    }

    /// When this target planning ended.
    pub fn planning_end_time(&self) -> DateTime<Utc> {
        self.planning_end_time
    }

    /// The target this spec will build.
    pub fn target(&self) -> &ConcreteTarget {
        &self.target
    }

    /// The required dependencies to build this spec and run any output artifacts.
    pub fn deps(&self) -> &Dependencies {
        &self.deps
    }
}

#[derive(Error, Debug)]
pub enum ExecutableSpecError {
    #[error(transparent)]
    BuilderError(derive_builder::UninitializedFieldError),
}

impl From<derive_builder::UninitializedFieldError> for ExecutableSpecError {
    fn from(value: derive_builder::UninitializedFieldError) -> Self {
        ExecutableSpecError::BuilderError(value)
    }
}
