use super::local_store::*;
use super::remote_store::*;
use super::*;
use dashmap::DashMap;
use std::path::PathBuf;
use std::sync::Arc;
use thiserror::*;
use tracing::*;

pub type StoreKey = String;

/// A lock to a store, represented as a physical file.
///
/// This lock file will be removed whenever the lock itself is out of scope / dropped.
///
pub struct StoreLock(PathBuf);

impl StoreLock {
    pub async fn try_lock(key: &PathBuf) -> Result<Self, std::io::Error> {
        tokio::fs::create_dir_all(&key).await?;
        let lock_file = key.join("Warp.lock");
        tokio::fs::write(&lock_file, "lock").await?;
        Ok(Self(lock_file))
    }
}

impl Drop for StoreLock {
    fn drop(&mut self) {
        let _ = std::fs::remove_file(&self.0);
    }
}

#[derive(Debug, Clone)]
pub struct Store {
    workspace_prefixes: DashMap<PathBuf, PathBuf>,
    local_outputs_root: PathBuf,
    local_store: LocalStore,
    remote_store: RemoteStore,
}

#[derive(Debug, Clone)]
pub enum StoreHitType {
    Miss(PathBuf),
    Hit(Box<TargetManifest>),
}

#[derive(Error, Debug)]
pub enum StoreError {
    #[error(transparent)]
    IOError(std::io::Error),

    #[error(transparent)]
    HTTPError(reqwest::Error),

    #[error(transparent)]
    ApiError(ApiError),

    #[error("When building {:?}, could not create Manifest file due to: {err:?}", target.label.to_string())]
    CouldNotCreateManifest {
        target: Box<ExecutableTarget>,
        err: TargetManifestError,
    },

    #[error(transparent)]
    TargetManifestError(TargetManifestError),

    #[error(transparent)]
    OutputManifestError(OutputManifestError),

    #[error("When promote outputs for {:?}, could not promote {src:?} into {dst:?} due to: {err:?}", label.to_string())]
    PromoteOutputError {
        label: Label,
        src: PathBuf,
        dst: PathBuf,
        err: std::io::Error,
    },

    #[error("Could not lock store key: {key:?}, due to: {err:?}")]
    LockError { key: PathBuf, err: std::io::Error },
}

impl Store {
    #[tracing::instrument(name = "Store::new", skip(workspace))]
    pub fn new(workspace: &Workspace, event_channel: Arc<EventChannel>) -> Self {
        Store {
            local_outputs_root: workspace.paths.local_outputs_root.clone(),
            local_store: LocalStore::new(workspace),
            remote_store: RemoteStore::new(workspace, event_channel),
            workspace_prefixes: {
                let map = DashMap::new();
                map.insert(
                    workspace.paths.workspace_root.clone(),
                    PathBuf::from(workspace.paths.workspace_name.clone()),
                );
                map
            },
        }
    }

    // TODO(@ostera): remove me
    pub fn register_workspace_raw(&self, prefix: PathBuf, root: PathBuf) {
        self.workspace_prefixes.insert(prefix, root);
    }

    pub fn register_workspace(&self, workspace: &Workspace) {
        self.workspace_prefixes.insert(
            workspace.paths.workspace_root.clone(),
            PathBuf::from(workspace.paths.workspace_name.clone()),
        );
    }

    pub fn _store_key(&self, hash: &str, label: &Label) -> StoreKey {
        if label.is_remote() {
            format!("{}/{}-{}", label.as_store_prefix(), hash, label.name())
        } else {
            self.workspace_prefixes
                .get(&label.workspace())
                .unwrap()
                .join(hash)
                .to_str()
                .unwrap()
                .to_string()
        }
    }

    pub fn store_key(&self, node: &ExecutableTarget) -> StoreKey {
        self._store_key(&node.hash, &node.label)
    }

    pub fn store_key_for_dep(&self, manifest: &TargetManifest) -> StoreKey {
        self._store_key(&manifest.hash, &manifest.label)
    }

    #[tracing::instrument(name = "Store::save")]
    pub async fn save(
        &self,
        node: &ExecutableTarget,
        manifest: &TargetManifest,
    ) -> Result<(), StoreError> {
        let store_key = self.store_key(node);

        let local_path = self.local_store.absolute_path_for_key(&store_key).await?;
        let mut artifacts = manifest.outs.clone();
        artifacts.push(PathBuf::from(MANIFEST_FILE));

        self.local_store
            .write_manifest(&local_path, manifest, node)
            .await?;

        let _ = self
            .remote_store
            .save(&store_key, &artifacts, &local_path)
            .await;

        Ok(())
    }

    /// Determine if a given node has been stored already or not.
    ///
    #[tracing::instrument(name = "Store::is_stored", skip(node))]
    pub async fn is_stored(&self, node: &ExecutableTarget) -> Result<StoreHitType, StoreError> {
        let store_key = self.store_key(node);

        match self.local_store.find_manifest(&store_key).await? {
            StoreHitType::Miss(_) => {
                let expected_path = self.local_store.absolute_path_for_key(&store_key).await?;
                let _ = self
                    .remote_store
                    .try_fetch(&store_key, &expected_path, &node.label)
                    .await;

                let result = self.local_store.find_manifest(&store_key).await;
                if let Ok(StoreHitType::Hit(manifest)) = &result {
                    self.promote_outputs(manifest).await?;
                }
                result
            }
            result => Ok(result),
        }
    }

    #[tracing::instrument(name = "Store::lock", skip(node))]
    pub async fn lock(&self, node: &ExecutableTarget) -> Result<StoreLock, StoreError> {
        let key = self.absolute_path_by_node(node).await?;
        StoreLock::try_lock(&key)
            .await
            .map_err(|err| StoreError::LockError { key, err })
    }

    #[tracing::instrument(name = "Store::clean", skip(node))]
    pub async fn clean(&self, node: &ExecutableTarget) -> Result<(), StoreError> {
        let store_key = self.store_key(node);
        self.local_store.clean(&store_key).await
    }

    #[tracing::instrument(name = "Store::absolute_path_by_dep", skip(dep))]
    pub async fn absolute_path_by_dep(&self, dep: &TargetManifest) -> Result<PathBuf, StoreError> {
        let store_key = self.store_key_for_dep(dep);
        self.local_store.absolute_path_for_key(&store_key).await
    }

    #[tracing::instrument(name = "Store::absolute_path_by_node", skip(node))]
    pub async fn absolute_path_by_node(
        &self,
        node: &ExecutableTarget,
    ) -> Result<PathBuf, StoreError> {
        let store_key = self.store_key(node);
        self.local_store.absolute_path_for_key(&store_key).await
    }

    #[tracing::instrument(name = "Store::promote_outputs", skip(manifest))]
    pub async fn promote_outputs(&self, manifest: &TargetManifest) -> Result<(), StoreError> {
        let store_key = self.store_key_for_dep(manifest);
        self.local_store
            .promote_outputs(&store_key, manifest, &self.local_outputs_root)
            .await
    }
}

#[cfg(test)]
mod tests {
    use super::*;
}
