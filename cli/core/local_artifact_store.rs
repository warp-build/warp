use super::*;
use fxhash::*;
use std::path::{Path, PathBuf};
use tokio::fs;
use tracing::*;

/// The LocalArtifactStore implements an in-memory and persisted cache for build nodes
/// based on their hashes.
///
#[derive(Debug, Clone)]
pub struct LocalArtifactStore {
    cache_root: PathBuf,
}

impl LocalArtifactStore {
    #[tracing::instrument(name = "LocalArtifactStore::new", skip(workspace))]
    pub fn new(workspace: &Workspace) -> LocalArtifactStore {
        LocalArtifactStore {
            cache_root: workspace.paths.global_cache_root.clone(),
        }
    }

    /// Determine if a given node has been cached already or not.
    ///
    /// This is based on hash of the node (see `BuildRule::hash`).
    ///
    /// FIXME: check if the expected hashes of the inputs match the actual
    /// hash of the files to determine if the cache is corrupted.
    #[tracing::instrument(name = "LocalArtifactStore::find_manifest")]
    pub async fn find_manifest(
        &self,
        key: &ArtifactStoreKey,
    ) -> Result<ArtifactStoreHitType, ArtifactStoreError> {
        let cache_path = self.cache_root.join(&key).join(MANIFEST_FILE);
        if fs::metadata(&cache_path).await.is_ok() {
            return match TargetManifest::from_file(&cache_path).await {
                Ok(manifest) => Ok(ArtifactStoreHitType::Hit(Box::new(manifest))),
                Err(err) => Err(ArtifactStoreError::TargetManifestError(err)),
            };
        }
        Ok(ArtifactStoreHitType::Miss(cache_path))
    }

    #[tracing::instrument(name = "LocalArtifactStore::write_manifest")]
    pub async fn write_manifest(
        &self,
        store_path: &Path,
        manifest: &TargetManifest,
        target: &ExecutableTarget,
    ) -> Result<(), ArtifactStoreError> {
        manifest
            .write(store_path)
            .await
            .map_err(|err| ArtifactStoreError::CouldNotCreateManifest {
                target: Box::new(target.clone()),
                err,
            })
    }

    #[tracing::instrument(name = "LocalArtifactStore::promote_outputs", skip(manifest))]
    pub async fn promote_outputs(
        &self,
        key: &ArtifactStoreKey,
        manifest: &TargetManifest,
        dst: &PathBuf,
    ) -> Result<(), ArtifactStoreError> {
        if let Ok(output_manifest) = OutputManifestHash::find(&manifest.label, dst).await {
            debug!(
                "Found output manifest for {} with hash {} (expected {})",
                manifest.label.to_string(),
                output_manifest.hash,
                manifest.hash
            );
            if output_manifest.hash == manifest.hash {
                debug!("Hashes match! Nothing to be removed");
                return Ok(());
            }

            // NOTE(@ostera): only if the hashes are different and we need to clean up do we
            // actually read the entire manifest.
            let output_manifest = OutputManifest::find(&manifest.label, dst)
                .await
                .map_err(ArtifactStoreError::OutputManifestError)?;

            debug!("Hashes do not match, removing old files...",);

            let mut rm_tasks = vec![];
            for out in &output_manifest.outs {
                rm_tasks.push(async move { fs::remove_file(&dst.join(out)).await });
            }
            for (result, file) in futures::future::join_all(rm_tasks)
                .await
                .into_iter()
                .zip(output_manifest.outs.iter())
            {
                match result {
                    Err(err) if err.kind() == std::io::ErrorKind::NotFound => continue,
                    _ => (),
                }

                result.map_err(|err| ArtifactStoreError::StaleOutputRemovalError {
                    err,
                    file: file.to_path_buf(),
                })?;
            }
        }

        debug!("Promoting outputs for {}", manifest.label.to_string());
        let hash_path = self.cache_root.join(key);

        let mut outs: FxHashMap<PathBuf, PathBuf> = FxHashMap::default();
        for out in &manifest.outs {
            outs.insert(hash_path.join(&out), dst.join(&out).to_path_buf());
        }

        for (src, dst) in &outs {
            // NOTE(@ostera): if the file doesn't exist, we just move on
            let _ = fs::remove_file(&dst).await;

            match fs::create_dir_all(&dst.parent().unwrap()).await {
                Ok(_) => Ok(()),
                Err(err) if err.kind() == std::io::ErrorKind::AlreadyExists => Ok(()),
                Err(err) => return Err(ArtifactStoreError::IOError(err)),
            }?;

            #[cfg(target_os = "windows")]
            let result = std::os::windows::fs::symlink(&src, &dst);

            #[cfg(not(target_os = "windows"))]
            let result = std::os::unix::fs::symlink(&src, &dst);

            match result {
                Ok(_) => Ok(()),
                Err(err) if err.kind() == std::io::ErrorKind::AlreadyExists => Ok(()),
                Err(err) => {
                    return Err(ArtifactStoreError::PromoteOutputError {
                        label: manifest.label.clone(),
                        src: src.to_path_buf(),
                        dst: dst.to_path_buf(),
                        err,
                    })
                }
            }?;
        }

        let output_manifest = OutputManifest {
            hash: manifest.hash.clone(),
            label: manifest.label.clone(),
            outs: outs.into_iter().map(|(_src, dst)| dst).collect(),
        };

        output_manifest
            .write(dst)
            .await
            .map_err(ArtifactStoreError::OutputManifestError)?;

        Ok(())
    }

    #[tracing::instrument(name = "LocalArtifactStore::needs_promotion", skip(manifest, dst))]
    async fn needs_promotion(
        &self,
        manifest: &TargetManifest,
        dst: &Path,
    ) -> Result<bool, ArtifactStoreError> {
        let buildstamp = dst.join(format!("{}.{}", manifest.label.hash(), BUILDSTAMP));

        if fs::metadata(&buildstamp).await.is_ok() {
            let hash = fs::read_to_string(buildstamp)
                .await
                .map_err(ArtifactStoreError::IOError)?;
            return Ok(hash != manifest.hash);
        }
        Ok(true)
    }

    #[tracing::instrument(name = "LocalArtifactStore::absolute_path_for_key")]
    pub async fn absolute_path_for_key(
        &self,
        key: &ArtifactStoreKey,
    ) -> Result<PathBuf, ArtifactStoreError> {
        fs::canonicalize(&self.cache_root)
            .await
            .map_err(ArtifactStoreError::IOError)
            .map(|p| p.join(key))
    }

    #[tracing::instrument(name = "LocalArtifactStore::clean")]
    pub async fn clean(&self, key: &ArtifactStoreKey) -> Result<(), ArtifactStoreError> {
        let cache_path = self.cache_root.join(&key);
        let _ = fs::remove_dir_all(&cache_path).await;
        let _ = fs::create_dir_all(&cache_path).await;
        trace!("Cleaned from cache: {:?}", &cache_path);
        Ok(())
    }
}

#[cfg(test)]
mod tests {}
