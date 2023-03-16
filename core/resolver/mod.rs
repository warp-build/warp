mod boot_resolver;
mod default;
mod error;
mod fs_resolver;
mod net_resolver;
mod target_registry;

pub use default::*;
pub use error::*;
use fs_resolver::*;
pub use target_registry::*;

use crate::model::{Goal, Signature, Target, TargetId};
use crate::sync::*;
use async_trait::async_trait;
use std::fmt::Debug;

#[derive(Debug)]
pub enum ResolutionFlow {
    Resolved { signature: Signature },
    IncompatibleTarget,
    MissingDeps { deps: Vec<TargetId> },
    IgnoredTarget(TargetId),
}

#[async_trait]
pub trait Resolver: Sync + Send + Clone {
    async fn resolve(
        &self,
        goal: Goal,
        target_id: TargetId,
        target: Arc<Target>,
    ) -> Result<ResolutionFlow, ResolverError>;
}
