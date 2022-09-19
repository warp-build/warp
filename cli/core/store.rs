use super::local_store::*;
use super::remote_store::*;
use super::*;
use std::path::PathBuf;
use thiserror::*;
use tracing::*;

pub type StoreKey = String;

#[derive(Debug, Clone)]
pub struct Store {
    workspace_prefix: String,
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

    #[error("When promote outputs for {:?}, could not promote {src:?} into {dst:?} due to: {err:?}", label.to_string())]
    PromoteOutputError {
        label: Label,
        src: PathBuf,
        dst: PathBuf,
        err: std::io::Error,
    },
}

impl Store {
    #[tracing::instrument(name = "Store::new", skip(workspace))]
    pub fn new(workspace: &Workspace) -> Self {
        Store {
            local_outputs_root: workspace.paths.local_outputs_root.clone(),
            local_store: LocalStore::new(workspace),
            remote_store: RemoteStore::new(workspace),
            workspace_prefix: workspace.paths.workspace_name.clone(),
        }
    }

    pub fn _store_key(&self, hash: &str, label: &Label) -> StoreKey {
        if label.is_remote() {
            format!("{}/{}-{}", label.as_store_prefix(), hash, label.name())
        } else {
            format!("{}/{}", self.workspace_prefix, hash)
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
        artifacts.push(PathBuf::from("Manifest.toml"));

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
                    .try_fetch(&store_key, &expected_path)
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
