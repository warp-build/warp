use super::*;
use fxhash::*;
use std::path::{Path, PathBuf};
use tokio::fs;
use tracing::*;

/// The LocalStore implements an in-memory and persisted cache for build nodes
/// based on their hashes.
///
#[derive(Debug, Clone)]
pub struct LocalStore {
    cache_root: PathBuf,
}

impl LocalStore {
    #[tracing::instrument(name = "LocalStore::new", skip(workspace))]
    pub fn new(workspace: &Workspace) -> LocalStore {
        LocalStore {
            cache_root: workspace.paths.global_cache_root.clone(),
        }
    }

    /// Determine if a given node has been cached already or not.
    ///
    /// This is based on hash of the node (see `BuildRule::hash`).
    ///
    /// FIXME: check if the expected hashes of the inputs match the actual
    /// hash of the files to determine if the cache is corrupted.
    #[tracing::instrument(name = "LocalStore::find_manifest")]
    pub async fn find_manifest(&self, key: &StoreKey) -> Result<StoreHitType, StoreError> {
        let cache_path = self.cache_root.join(&key).join("Manifest.toml");
        if fs::metadata(&cache_path).await.is_ok() {
            let manifest = TargetManifest::from_file(&cache_path)
                .await
                .map_err(StoreError::TargetManifestError)?;

            return Ok(StoreHitType::Hit(manifest));
        }
        Ok(StoreHitType::Miss(cache_path))
    }

    #[tracing::instrument(name = "LocalStore::write_manifest")]
    pub async fn write_manifest(
        &self,
        store_path: &Path,
        manifest: &TargetManifest,
        target: &ExecutableTarget,
    ) -> Result<(), StoreError> {
        manifest
            .write(store_path)
            .await
            .map_err(|err| StoreError::CouldNotCreateManifest {
                target: Box::new(target.clone()),
                err,
            })
    }

    #[tracing::instrument(name = "LocalStore::promote_outputs", skip(node))]
    pub async fn promote_outputs(
        &self,
        key: &StoreKey,
        node: &ExecutableTarget,
        dst: &PathBuf,
    ) -> Result<(), StoreError> {
        trace!("Promoting outputs for {}", node.label.to_string());

        let hash_path = self.cache_root.join(key);

        let mut paths: FxHashMap<PathBuf, ()> = FxHashMap::default();
        let mut outs: FxHashMap<PathBuf, PathBuf> = FxHashMap::default();
        for out in &node.outs {
            paths.insert(dst.join(&out).parent().unwrap().to_path_buf(), ());
            outs.insert(hash_path.join(&out), dst.join(&out).to_path_buf());
        }

        for (path, _) in paths {
            fs::create_dir_all(&path)
                .await
                .map_err(StoreError::IOError)?;
        }

        for (src, dst) in outs {
            let _ = fs::remove_file(&dst).await;

            #[cfg(target_os = "windows")]
            match std::os::windows::fs::symlink(&src, &dst) {
                Ok(_) => Ok(()),
                Err(err) if err.kind() == std::io::ErrorKind::AlreadyExists => Ok(()),
                Err(err) => Err(StoreError::IOError(err)),
            }?;

            #[cfg(not(target_os = "windows"))]
            match std::os::unix::fs::symlink(&src, &dst) {
                Ok(_) => Ok(()),
                Err(err) if err.kind() == std::io::ErrorKind::AlreadyExists => Ok(()),
                Err(err) => Err(StoreError::IOError(err)),
            }?;
        }

        Ok(())
    }

    #[tracing::instrument(name = "LocalStore::absolute_path_for_key")]
    pub async fn absolute_path_for_key(&self, key: &StoreKey) -> Result<PathBuf, StoreError> {
        fs::canonicalize(&self.cache_root)
            .await
            .map_err(StoreError::IOError)
            .map(|p| p.join(key))
    }

    #[tracing::instrument(name = "LocalStore::clean")]
    pub async fn clean(&self, key: &StoreKey) -> Result<(), StoreError> {
        let cache_path = self.cache_root.join(&key);
        let _ = fs::remove_dir_all(&cache_path).await;
        let _ = fs::create_dir_all(&cache_path).await;
        trace!("Cleaned from cache: {:?}", &cache_path);
        Ok(())
    }
}

#[cfg(test)]
mod tests {}
