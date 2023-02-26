use super::{
    FsResolver, Goal, ResolutionFlow, Resolver, ResolverError, Target, TargetId, TargetRegistry,
};
use crate::sync::*;
use async_trait::async_trait;

#[derive(Clone, Debug)]
pub struct DefaultResolver {
    fs_resolver: Arc<FsResolver>,
}

impl DefaultResolver {
    pub fn new() -> Self {
        let fs_resolver = Arc::new(FsResolver::new());
        Self { fs_resolver }
    }
}

impl Resolver for DefaultResolver {
    async fn resolve(
        &self,
        goal: Goal,
        target: Arc<Target>,
    ) -> Result<ResolutionFlow, ResolverError> {
        let concrete_target = match target {
            // Target::Alias(a) => self.alias_resolver.resolve(goal, a).await?,
            // Target::Remote(r) => self.net_resolver.resolve(goal, r).await?,
            Target::Fs(f) => self.fs_resolver.resolve(goal, f).await?,
            _ => todo()!
        };

        // 1. find and ready the tricorder
        let tricorder = self.tricorder_manager.find_and_ready().await?;

        // 2. generate signature for this concrete target
        match tricorder.generate_signature(concrete_target).await? {
            

        }
        
    }
}