//! # Flexible Task Planning Engine

mod default;
mod default_planner_context;
mod error;

pub use default::*;
pub use default_planner_context::*;
pub use error::*;

use crate::model::{Dependencies, ExecutableSpec, ExecutionEnvironment, Signature, Task};
use async_trait::async_trait;

#[derive(Debug)]
pub enum PlanningFlow {
    Planned { spec: ExecutableSpec },
    MissingDeps { deps: Vec<Task> },
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
        task: Task,
        sig: &Signature,
        env: ExecutionEnvironment,
    ) -> Result<PlanningFlow, PlannerError>;
}
