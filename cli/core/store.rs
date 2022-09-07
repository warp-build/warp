use super::local_store::*;
use super::remote_store::*;
use super::*;
use std::path::PathBuf;
use tracing::*;

pub type StoreKey = String;

#[derive(Debug, Clone)]
pub struct Store {
    workspace_prefix: String,
    local_store: LocalStore,
    remote_store: RemoteStore,
}

#[derive(Debug, Clone)]
pub enum StoreHitType {
    Miss(PathBuf),
    Hit(PathBuf),
}

impl Store {
    #[tracing::instrument(name = "Store::new", skip(workspace))]
    pub fn new(workspace: &Workspace) -> Self {
        Store {
            local_store: LocalStore::new(workspace),
            remote_store: RemoteStore::new(workspace),
            workspace_prefix: workspace.paths.workspace_name.clone(),
        }
    }

    pub fn _store_key(&self, hash: &str, label: &Label) -> StoreKey {
        if label.is_remote() {
            format!("{}-{}", label.as_store_prefix(), hash)
        } else {
            format!("{}/{}", self.workspace_prefix, hash)
        }
    }

    pub fn store_key(&self, node: &ExecutableTarget) -> StoreKey {
        self._store_key(&node.hash, &node.label)
    }

    pub fn store_key_for_dep(&self, dep: &Dependency) -> StoreKey {
        self._store_key(&dep.hash, &dep.label)
    }

    #[tracing::instrument(name = "Store::save", skip(sandbox))]
    pub async fn save(&mut self, sandbox: &LocalSandbox) -> Result<(), anyhow::Error> {
        let node = sandbox.node();
        let store_key = self.store_key(node);

        // NOTE(@ostera): see RFC0005
        let artifacts = if node.is_pinned() {
            sandbox.all_outputs().await
        } else {
            sandbox.outputs()
        };

        let _ = self
            .remote_store
            .save(&store_key, &artifacts, &sandbox.root())
            .await;

        Ok(())
    }

    /// Determine if a given node has been stored already or not.
    ///
    #[tracing::instrument(name = "Store::is_stored", skip(node))]
    pub async fn is_in_store(
        &mut self,
        node: &ExecutableTarget,
    ) -> Result<StoreHitType, anyhow::Error> {
        let store_key = self.store_key(node);

        match self.local_store.is_stored(&store_key).await? {
            StoreHitType::Miss(_) => {
                let expected_path = self.local_store.absolute_path_for_key(&store_key).await?;
                let _ = self
                    .remote_store
                    .try_fetch(&store_key, &expected_path)
                    .await;
                self.local_store.is_stored(&store_key).await
            }
            result => Ok(result),
        }
    }

    #[tracing::instrument(name = "Store::evict", skip(node))]
    pub async fn evict(&mut self, node: &ExecutableTarget) -> Result<(), anyhow::Error> {
        let store_key = self.store_key(node);
        self.local_store.evict(&store_key).await
    }

    #[tracing::instrument(name = "Store::absolute_path_by_dep")]
    pub async fn absolute_path_by_dep(&self, dep: &Dependency) -> Result<PathBuf, anyhow::Error> {
        let store_key = self.store_key_for_dep(dep);
        self.local_store.absolute_path_for_key(&store_key).await
    }

    #[tracing::instrument(name = "Store::absolute_path_by_node")]
    pub async fn absolute_path_by_node(
        &self,
        node: &ExecutableTarget,
    ) -> Result<PathBuf, anyhow::Error> {
        let store_key = self.store_key(node);
        self.local_store.absolute_path_for_key(&store_key).await
    }

    #[tracing::instrument(name = "Store::promote_outputs", skip(node))]
    pub async fn promote_outputs(
        &self,
        node: &ExecutableTarget,
        dst: &PathBuf,
    ) -> Result<(), anyhow::Error> {
        let store_key = self.store_key(node);
        self.local_store
            .promote_outputs(&store_key, node, dst)
            .await
    }
}
