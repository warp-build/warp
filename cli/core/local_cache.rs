use super::*;
use anyhow::{anyhow, Context};
use fxhash::*;
use tracing::*;
use std::path::PathBuf;

/// The LocalCache implements an in-memory and persisted cache for build nodes
/// based on their hashes.
///
#[derive(Debug, Clone)]
pub struct LocalCache {
    global_root: PathBuf,
    local_root: PathBuf,
}

#[derive(Debug, Clone)]
pub enum CacheHitType {
    Miss,
    Global,
    Local,
}

impl LocalCache {
    #[tracing::instrument(name="LocalCache::new", skip(workspace))]
    pub fn new(workspace: &Workspace) -> LocalCache {
        LocalCache {
            global_root: workspace.paths.global_cache_root.clone(),
            local_root: workspace.paths.local_cache_root.clone(),
        }
    }

    #[tracing::instrument(name="LocalCache::save", skip(sandbox))]
    pub fn save(&mut self, sandbox: &LocalSandbox) -> Result<(), anyhow::Error> {
        let node = sandbox.node();
        let hash = node.hash();
        let cache_path = if node.target.is_local() {
            self.local_root.join(&hash)
        } else {
            self.global_root.join(&hash)
        };

        debug!(
            "Caching node {:?} hashed {:?}: {:?} outputs",
            node.label(),
            &hash,
            sandbox.outputs().len()
        );

        sandbox
            .outputs()
            .iter()
            .cloned()
            .map(|artifact| {
                debug!("Caching build artifact: {:?}", &artifact);
                let cached_file = cache_path.join(&artifact);

                // ensure all dirs are there for this file
                let cached_dir = &cached_file.parent().unwrap();
                debug!("Creating artifact cache path: {:?}", &cached_dir);
                std::fs::create_dir_all(&cached_dir)
                    .context(format!(
                        "Could not prepare directory for artifact {:?} into cache path: {:?}",
                        &artifact, &cached_dir
                    ))
                    .map(|_| ())?;

                let sandboxed_artifact = &sandbox.root().join(artifact);
                debug!(
                    "Moving artifact from sandbox path {:?} to cache path {:?}",
                    &sandboxed_artifact, &cached_file
                );
                std::fs::rename(&sandboxed_artifact, &cached_file)
                    .context(format!(
                        "Could not move artifact {:?} into cache path: {:?}",
                        sandboxed_artifact, cached_file
                    ))
                    .map(|_| ())?;

                Ok(())
            })
            .collect::<Result<(), anyhow::Error>>()
    }

    #[tracing::instrument(name="LocalCache::promote_outputs", skip(node))]
    pub fn promote_outputs(
        &self,
        node: &ComputedTarget,
        dst: &PathBuf,
    ) -> Result<(), anyhow::Error> {
        trace!("Promoting outputs for {}", node.target.label().to_string());
        let hash = node.hash();
        let hash_path = if node.target.is_local() {
            self.local_root.join(&hash)
        } else {
            self.global_root.join(&hash)
        };

        let mut paths: FxHashMap<PathBuf, ()> = FxHashMap::default();
        let mut outs: FxHashMap<PathBuf, PathBuf> = FxHashMap::default();
        for out in node.outs() {
            paths.insert(dst.join(&out).parent().unwrap().to_path_buf(), ());
            outs.insert(hash_path.join(&out), dst.join(&out).to_path_buf());
        }

        for (path, _) in paths {
            std::fs::create_dir_all(&path)?;
        }
        for (src, dst) in outs {
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

    #[tracing::instrument(name="LocalCache::absolute_path_by_hash")]
    pub fn absolute_path_by_hash(&self, hash: &str) -> PathBuf {
        match std::fs::canonicalize(&self.local_root.join(hash)) {
            Err(_) => {
                let path = self.global_root.join(hash);
                std::fs::canonicalize(&path).unwrap_or_else(|_| {
                    panic!(
                        "Could not find {:?} in disk, has the cache been modified manually?",
                        &path
                    )
                })
            }
            Ok(path) => path,
        }
    }

    /// Determine if a given node has been cached already or not.
    ///
    /// This is based on hash of the node (see `BuildRule::hash`).
    ///
    /// FIXME: check if the expected hashes of the inputs match the actual
    /// hash of the files to determine if the cache is corrupted.
    #[tracing::instrument(name="LocalCache::is_cached", skip(node))]
    pub fn is_cached(&mut self, node: &ComputedTarget) -> Result<CacheHitType, anyhow::Error> {
        let hash = node.hash();

        let hash_path = self.local_root.join(&hash);
        debug!("Checking if {:?} is in the cache...", hash_path);
        if std::fs::metadata(&hash_path).is_ok() {
            debug!(
                "Cache hit for {} at {:?}",
                node.label().to_string(),
                hash_path
            );
            return Ok(CacheHitType::Local);
        }

        let hash_path = self.global_root.join(&hash);
        debug!("Checking if {:?} is in the cache...", hash_path);
        if std::fs::metadata(&hash_path).is_ok() {
            debug!(
                "Cache hit for {} at {:?}",
                node.label().to_string(),
                hash_path
            );
            return Ok(CacheHitType::Global);
        }

        let named_path = self
            .global_root
            .join(format!("{}-{}", node.label().name(), &hash));
        debug!("Checking if {:?} is in the cache...", named_path);
        if std::fs::metadata(&named_path).is_ok() {
            debug!(
                "Cache hit for {} at {:?}",
                node.label().to_string(),
                named_path
            );
            return Ok(CacheHitType::Global);
        }

        debug!("No cache hit for {}", node.label().to_string());
        Ok(CacheHitType::Miss)
    }

    #[tracing::instrument(name="LocalCache::evict", skip(node))]
    pub fn evict(&mut self, node: &ComputedTarget) -> Result<(), anyhow::Error> {
        let hash = node.hash();

        let hash_path = self.local_root.join(&hash);
        debug!("Checking if {:?} is in the cache...", &hash_path);
        if std::fs::metadata(&hash_path).is_ok() {
            trace!("Found it, removing...");
            std::fs::remove_dir_all(&hash_path)?;
        }
        trace!("Cleaned from cache: {:?}", &hash_path);
        Ok(())
    }
}
