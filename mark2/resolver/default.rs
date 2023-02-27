use super::{
    ConcreteTarget, FsResolver, Goal, ResolutionFlow, Resolver, ResolverError, Target, TargetId,
    TargetRegistry,
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

    async fn concretize_target(
        &self,
        goal: Goal,
        target: Arc<Target>,
    ) -> Result<ConcreteTarget, ResolverError> {
        let final_path = match &*target {
            // Target::Alias(a) => self.alias_resolver.resolve(goal, a).await?,
            // Target::Remote(r) => self.net_resolver.resolve(goal, r).await?,
            Target::Fs(f) => self.fs_resolver.resolve(goal, f).await?,
            _ => todo!(),
        };
        Ok(ConcreteTarget::new(goal, target, final_path))
    }
}

impl Resolver for DefaultResolver {
    async fn resolve(
        &self,
        goal: Goal,
        target: Arc<Target>,
    ) -> Result<ResolutionFlow, ResolverError> {
        let concrete_target = self.concretize_target(goal, target).await?;

        // 1. find and ready the tricorder
        let tricorder = self.tricorder_manager.find_and_ready(concrete_target).await?;

        // 2. generate signature for this concrete target
        match tricorder.generate_signature(concrete_target).await? {}
    }
}
