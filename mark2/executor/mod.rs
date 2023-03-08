//! # Incremental Task Execution Engine
//!
pub mod actions;
mod error;
pub mod local;

pub use error::*;

use crate::model::ExecutableSpec;
use crate::store::ArtifactManifest;
use crate::sync::*;
use async_trait::async_trait;
use std::path::PathBuf;

pub enum ExecutionFlow {
    Completed(Arc<ArtifactManifest>),
    ValidationError(ValidationStatus),
}

pub enum ValidationStatus {
    Valid {
        outputs: Vec<PathBuf>,
    },
    Invalid {
        store_path: PathBuf,
        expected_and_present: Vec<PathBuf>,
        unexpected_but_present: Vec<PathBuf>,
        expected_but_missing: Vec<PathBuf>,
    },
}

#[async_trait]
pub trait Executor {
    type Context: Sync + Send + Clone + Sized;

    fn new(ctx: Self::Context) -> Result<Self, ExecutorError>
    where
        Self: Sized;

    async fn execute(&mut self, spec: &ExecutableSpec) -> Result<ExecutionFlow, ExecutorError>;
}
