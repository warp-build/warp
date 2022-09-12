use super::*;
use anyhow::{anyhow, Context};
use fxhash::*;
use std::path::PathBuf;
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

    #[tracing::instrument(name = "LocalStore::save", skip(artifacts))]
    pub async fn save(
        &mut self,
        key: &StoreKey,
        artifacts: &[PathBuf],
        sandbox_root: &PathBuf,
    ) -> Result<(), anyhow::Error> {
        let cache_path = self.cache_root.join(key);

        for artifact in artifacts {
            debug!("Caching build artifact: {:?}", &artifact);
            let cached_file = cache_path.join(&artifact);

            // ensure all dirs are there for this file
            let cached_dir = &cached_file.parent().unwrap();
            debug!("Creating artifact cache path: {:?}", &cached_dir);
            fs::create_dir_all(&cached_dir)
                .await
                .context(format!(
                    "Could not prepare directory for artifact {:?} into cache path: {:?}",
                    &artifact, &cached_dir
                ))
                .map(|_| ())?;

            let sandboxed_artifact = &sandbox_root.join(artifact);
            debug!(
                "Moving artifact from sandbox path {:?} to cache path {:?}",
                &sandboxed_artifact, &cached_file
            );
            fs::copy(&sandboxed_artifact, &cached_file)
                .await
                .context(format!(
                    "Could not move artifact {:?} into cache path: {:?}",
                    sandboxed_artifact, cached_file
                ))
                .map(|_| ())?;
        }

        Ok(())
    }

    #[tracing::instrument(name = "LocalStore::promote_outputs", skip(node))]
    pub async fn promote_outputs(
        &self,
        key: &StoreKey,
        node: &ExecutableTarget,
        dst: &PathBuf,
    ) -> Result<(), anyhow::Error> {
        trace!("Promoting outputs for {}", node.label.to_string());

        let hash_path = self.cache_root.join(key);

        let mut paths: FxHashMap<PathBuf, ()> = FxHashMap::default();
        let mut outs: FxHashMap<PathBuf, PathBuf> = FxHashMap::default();
        for out in &node.outs {
            paths.insert(dst.join(&out).parent().unwrap().to_path_buf(), ());
            outs.insert(hash_path.join(&out), dst.join(&out).to_path_buf());
        }

        for (path, _) in paths {
            fs::create_dir_all(&path).await?;
        }
        for (src, dst) in outs {
            let _ = fs::remove_file(&dst).await;

            #[cfg(target_os = "windows")]
            match std::os::windows::fs::symlink(&src, &dst) {
                Ok(_) => Ok(()),
                Err(err) if err.kind() == std::io::ErrorKind::AlreadyExists => Ok(()),
                err => Err(anyhow!("Could not create symlink because of: {:?}", err)),
            }?;

            #[cfg(not(target_os = "windows"))]
            match std::os::unix::fs::symlink(&src, &dst) {
                Ok(_) => Ok(()),
                Err(err) if err.kind() == std::io::ErrorKind::AlreadyExists => Ok(()),
                err => Err(anyhow!("Could not create symlink because of: {:?}", err)),
            }?;
        }

        Ok(())
    }

    #[tracing::instrument(name = "LocalStore::absolute_path_for_key")]
    pub async fn absolute_path_for_key(&self, key: &StoreKey) -> Result<PathBuf, anyhow::Error> {
        fs::canonicalize(&self.cache_root)
            .await
            .map_err(|_| {
                anyhow!(
                    "Could not find {:?} in disk, has the cache been modified manually?",
                    &self.cache_root
                )
            })
            .map(|p| p.join(key))
    }

    /// Determine if a given node has been cached already or not.
    ///
    /// This is based on hash of the node (see `BuildRule::hash`).
    ///
    /// FIXME: check if the expected hashes of the inputs match the actual
    /// hash of the files to determine if the cache is corrupted.
    #[tracing::instrument(name = "LocalStore::is_stored")]
    pub async fn is_stored(&mut self, key: &StoreKey) -> Result<StoreHitType, anyhow::Error> {
        let cache_path = self.cache_root.join(&key);
        if fs::metadata(&cache_path).await.is_ok() {
            return Ok(StoreHitType::Hit(cache_path));
        }
        Ok(StoreHitType::Miss(cache_path))
    }

    #[tracing::instrument(name = "LocalStore::evict")]
    pub async fn evict(&mut self, key: &StoreKey) -> Result<(), anyhow::Error> {
        let cache_path = self.cache_root.join(&key);
        debug!("Checking if {:?} is in the cache...", &cache_path);
        if fs::metadata(&cache_path).await.is_ok() {
            trace!("Found it, removing...");
            fs::remove_dir_all(&cache_path).await?;
        }
        trace!("Cleaned from cache: {:?}", &cache_path);
        Ok(())
    }
}
