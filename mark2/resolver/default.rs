use super::{Goal, ResolutionFlow, Resolver, ResolverError, TargetId};
use async_trait::async_trait;

#[derive(Clone, Debug)]
pub struct DefaultResolver;

impl DefaultResolver {
    pub fn new() -> Self {
        Self
    }
}

#[async_trait]
impl Resolver for DefaultResolver {
    async fn resolve(
        &self,
        goal: Goal,
        target_id: TargetId,
    ) -> Result<ResolutionFlow, ResolverError> {
        Ok(ResolutionFlow::MissingDependencies)
    }
}
