mod boot_resolver;
mod default;
mod error;
mod fs_resolver;
mod net_resolver;
mod signature_registry;
mod target_registry;

pub use default::*;
pub use error::*;
use fs_resolver::*;
pub use signature_registry::*;
pub use target_registry::*;

use crate::model::{SignatureId, Target, TargetId, Task};
use crate::sync::*;
use async_trait::async_trait;
use std::fmt::Debug;

#[derive(Debug)]
pub enum ResolutionFlow {
    Resolved { signature_ids: Vec<SignatureId> },
    IncompatibleTarget,
    MissingDeps { deps: Vec<Task> },
    IgnoredTarget(TargetId),
}

#[async_trait]
pub trait Resolver: Sync + Send + Clone {
    async fn resolve(
        &self,
        task: Task,
        target: Arc<Target>,
    ) -> Result<ResolutionFlow, ResolverError>;
}
