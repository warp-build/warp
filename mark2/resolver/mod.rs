//! # Resolves what a given target points to in the project graph
//!
mod default;
mod goal;
mod signature;
mod target;
mod target_id;
mod target_registry;

use std::fmt::Debug;

pub use default::*;
pub use goal::*;
pub use signature::*;
pub use target::*;
pub use target_id::*;
pub use target_registry::*;

use async_trait::async_trait;
use thiserror::*;

#[derive(Error, Debug)]
pub enum ResolverError {
    #[error("Something went wrong: {0}")]
    Unknown(String),
}

#[derive(Debug)]
pub enum ResolutionFlow {
    Resolved { signature: Signature },
    MissingDependencies,
}

#[async_trait]
pub trait Resolver: Sync + Send + Clone {
    async fn resolve(
        &self,
        goal: Goal,
        target_id: TargetId,
    ) -> Result<ResolutionFlow, ResolverError>;
}
