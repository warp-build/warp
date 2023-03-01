//! # Resolves what a given target points to in the project graph
//!
mod concrete_target;
mod default;
mod fs_resolver;
mod goal;
mod signature;
mod target;
mod target_id;
mod target_registry;

pub use concrete_target::*;
pub use default::*;
use fs_resolver::*;
pub use goal::*;
pub use signature::*;
pub use target::*;
pub use target_id::*;
pub use target_registry::*;

use crate::sync::*;
use crate::tricorder::{TricorderError, TricorderManagerError};
use async_trait::async_trait;
use std::fmt::Debug;
use thiserror::*;

#[derive(Error, Debug)]
pub enum ResolverError {
    #[error("Something went wrong: {0}")]
    Unknown(String),

    #[error("Could not resolve target {target:?} for goal {goal:?}")]
    CouldNotResolveTarget { goal: Goal, target: Target },

    #[error(transparent)]
    FsResolverError(FsResolverError),

    #[error(transparent)]
    TricorderManagerError(TricorderManagerError),

    #[error(transparent)]
    TricorderError(TricorderError),
}

impl From<TricorderError> for ResolverError {
    fn from(value: TricorderError) -> Self {
        Self::TricorderError(value)
    }
}

impl From<TricorderManagerError> for ResolverError {
    fn from(value: TricorderManagerError) -> Self {
        Self::TricorderManagerError(value)
    }
}

#[derive(Debug)]
pub enum ResolutionFlow {
    Resolved { signature: Signature },
    IncompatibleTarget,
    MissingDependencies,
}

#[async_trait]
pub trait Resolver: Sync + Send + Clone {
    async fn resolve(
        &self,
        goal: Goal,
        target: Arc<Target>,
    ) -> Result<ResolutionFlow, ResolverError>;
}
