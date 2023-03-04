use super::{GrpcTricorder, Tricorder, TricorderRegistry, TricorderRegistryError};
use crate::resolver::ConcreteTarget;
use crate::store::{Store, StoreError};
use crate::sync::*;
use crate::util::process_pool::ProcessPool;
use thiserror::*;

pub struct TricorderManager<T: Tricorder, S: Store> {
    registry: TricorderRegistry,
    process_pool: ProcessPool<T>,
    artifact_store: Arc<S>,
}

impl<T: Tricorder, S: Store> TricorderManager<T, S> {
    pub fn new(artifact_store: Arc<S>) -> Self {
        Self {
            registry: TricorderRegistry::new(),
            process_pool: ProcessPool::new(),
            artifact_store,
        }
    }

    pub async fn find_and_ready(
        &self,
        concrete_target: &ConcreteTarget,
    ) -> Result<GrpcTricorder, TricorderManagerError> {
        // 1. find exactly which tricorder we need
        let tricorder_url = self.registry.find_by_path(concrete_target.path()).await?;

        // 2. install it
        let artifact_manifest = self
            .artifact_store
            .install_from_manifest_url(&tricorder_url)
            .await?;

        // let handler_tricorder = self.process_pool.spawn(Spec { bin_path: manifest.provides.main_path }).await?;

        // tricorder.ensure_ready().await?;

        todo!()
    }
}

#[derive(Error, Debug)]
pub enum TricorderManagerError {
    #[error(transparent)]
    TricorderRegistryError(TricorderRegistryError),

    #[error(transparent)]
    StoreError(StoreError),
}

impl From<TricorderRegistryError> for TricorderManagerError {
    fn from(err: TricorderRegistryError) -> Self {
        Self::TricorderRegistryError(err)
    }
}

impl From<StoreError> for TricorderManagerError {
    fn from(value: StoreError) -> Self {
        Self::StoreError(value)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::store::{ManifestUrl, StoreError};
    use async_trait::async_trait;

    pub struct NoopStore;

    #[async_trait]
    impl Store for NoopStore {
        async fn install_from_manifest_url(&self, _url: &ManifestUrl) -> Result<(), StoreError> {
            todo!()
        }
    }

    #[tokio::test]
    async fn installs_a_tricorder_from_a_concrete_target() {
        // let store = Arc::new(NoopStore);
        // let mgr = TricorderManager::new(store);

        // let ct = ConcreteTarget::new(goal, original_target, path);
        assert!(true)
    }
}
