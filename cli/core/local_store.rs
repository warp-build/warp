use super::*;
use fxhash::*;
use serde_derive::{Deserialize, Serialize};
use std::path::{Path, PathBuf};
use thiserror::*;
use tokio::fs;
use tokio::io::AsyncReadExt;
use tracing::*;

pub const BUILDSTAMP: &str = "output.json";

#[derive(Error, Debug)]
pub enum OutputManifestError {
    #[error("Could not parse Manifest file: {0:?}")]
    ParseError(serde_json::Error),

    #[error("Could not print Manifest file: {0:#?}")]
    PrintError(serde_json::Error),

    #[error(transparent)]
    IOError(std::io::Error),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OutputManifest {
    pub label: Label,
    pub outs: Vec<PathBuf>,
}

impl OutputManifest {
    #[tracing::instrument(name = "OutputManifest::find")]
    pub async fn find(label: &Label, path: &PathBuf) -> Result<Self, OutputManifestError> {
        let mut file = fs::File::open(OutputManifest::_file(label, path))
            .await
            .map_err(OutputManifestError::IOError)?;

        let mut bytes = vec![];
        file.read_to_end(&mut bytes)
            .await
            .map_err(OutputManifestError::IOError)?;

        serde_json::from_slice(&bytes).map_err(OutputManifestError::ParseError)
    }

    #[tracing::instrument(name = "OutputManifest::write")]
    pub async fn write(&self, root: &PathBuf) -> Result<(), OutputManifestError> {
        let json = serde_json::to_string_pretty(&self).map_err(OutputManifestError::PrintError)?;

        let file = OutputManifest::_file(&self.label, root);

        fs::write(file, json)
            .await
            .map_err(OutputManifestError::IOError)
    }

    fn _file(label: &Label, root: &PathBuf) -> PathBuf {
        root.join(&label.hash().to_string())
            .with_extension(BUILDSTAMP)
    }
}

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
        let cache_path = self.cache_root.join(&key).join(MANIFEST_FILE);
        if fs::metadata(&cache_path).await.is_ok() {
            return match TargetManifest::from_file(&cache_path).await {
                Ok(manifest) => Ok(StoreHitType::Hit(Box::new(manifest))),
                Err(err) => Err(StoreError::TargetManifestError(err)),
            };
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

    #[tracing::instrument(name = "LocalStore::promote_outputs", skip(manifest))]
    pub async fn promote_outputs(
        &self,
        key: &StoreKey,
        manifest: &TargetManifest,
        dst: &PathBuf,
    ) -> Result<(), StoreError> {
        // if let Ok(output_manifest) = OutputManifest::find(&manifest.label, dst).await {
        //     if output_manifest.label.hash().to_string().eq(&manifest.hash) {
        //         return Ok(());
        //     }
        // }

        trace!("Promoting outputs for {}", manifest.label.to_string());
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
                Err(err) => return Err(StoreError::IOError(err)),
            }?;

            #[cfg(target_os = "windows")]
            let result = std::os::windows::fs::symlink(&src, &dst);

            #[cfg(not(target_os = "windows"))]
            let result = std::os::unix::fs::symlink(&src, &dst);

            match result {
                Ok(_) => Ok(()),
                Err(err) => {
                    return Err(StoreError::PromoteOutputError {
                        label: manifest.label.clone(),
                        src: src.to_path_buf(),
                        dst: dst.to_path_buf(),
                        err,
                    })
                }
            }?;
        }

        let output_manifest = OutputManifest {
            label: manifest.label.clone(),
            outs: outs.into_iter().map(|(_src, dst)| dst).collect(),
        };

        output_manifest
            .write(dst)
            .await
            .map_err(StoreError::OutputManifestError)?;

        Ok(())
    }

    #[tracing::instrument(name = "LocalStore::needs_promotion", skip(manifest, dst))]
    async fn needs_promotion(
        &self,
        manifest: &TargetManifest,
        dst: &PathBuf,
    ) -> Result<bool, StoreError> {
        let buildstamp = dst.join(format!("{}.{}", manifest.label.hash(), BUILDSTAMP));

        if fs::metadata(&buildstamp).await.is_ok() {
            let hash = fs::read_to_string(buildstamp)
                .await
                .map_err(StoreError::IOError)?;
            return Ok(hash != manifest.hash);
        }
        Ok(true)
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
