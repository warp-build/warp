use super::{FsResolver, ResolutionFlow, Resolver, ResolverError};
use crate::model::{ConcreteTarget, Goal, Target};
use crate::store::DefaultStore;
use crate::tricorder::{GrpcTricorder, SignatureGenerationFlow, TricorderManager};
use crate::{sync::*, Config};
use async_trait::async_trait;

#[derive(Clone)]
pub struct DefaultResolver {
    fs_resolver: Arc<FsResolver>,
    tricorder_manager: Arc<TricorderManager<GrpcTricorder, DefaultStore>>,
}

impl DefaultResolver {
    pub fn new(config: Config, store: Arc<DefaultStore>) -> Self {
        let fs_resolver = Arc::new(FsResolver::new());
        let tricorder_manager = Arc::new(TricorderManager::new(config, store));
        Self {
            fs_resolver,
            tricorder_manager,
        }
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

#[async_trait]
impl Resolver for DefaultResolver {
    async fn resolve(
        &self,
        goal: Goal,
        target: Arc<Target>,
    ) -> Result<ResolutionFlow, ResolverError> {
        let concrete_target = self.concretize_target(goal, target).await?;

        // 1. find and ready the tricorder
        let tricorder = self
            .tricorder_manager
            .find_and_ready(&concrete_target)
            .await?;

        // TODO(@ostera):  at this stage, we want to use the concrete target and the tricorder to
        // call the CodeManager and ask it to tree-split, so we can avoid regenerating signatures
        // if parts of the file we don't care about haven't changed.

        // 2. generate signature for this concrete target
        let sig_flow = tricorder.generate_signature(&concrete_target).await?;

        match sig_flow {
            SignatureGenerationFlow::GeneratedSignatures { signatures } if signatures.len() > 0 => {
                let signature = signatures.into_iter().nth(0).unwrap();
                Ok(ResolutionFlow::Resolved { signature })
            }
            SignatureGenerationFlow::MissingRequirements { requirements } => {
                Ok(ResolutionFlow::MissingDependencies { requirements })
            }
            _ => todo!(),
        }
    }
}
