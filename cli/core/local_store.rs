use super::*;
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

    /// Determine if a given node has been cached already or not.
    ///
    /// This is based on hash of the node (see `BuildRule::hash`).
    ///
    /// FIXME: check if the expected hashes of the inputs match the actual
    /// hash of the files to determine if the cache is corrupted.
    #[tracing::instrument(name = "LocalStore::is_stored")]
    pub async fn is_stored(&mut self, key: &StoreKey) -> Result<StoreHitType, StoreError> {
        let cache_path = self.cache_root.join(&key);
        if fs::metadata(&cache_path).await.is_ok() {
            return Ok(StoreHitType::Hit(cache_path));
        }
        Ok(StoreHitType::Miss(cache_path))
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
mod tests {
    use super::*;

    fn root() -> PathBuf {
        PathBuf::from(&env!("CARGO_MANIFEST_DIR")).join("tests")
    }

    async fn local_store() -> LocalStore {
        let paths = WorkspacePaths::new(
            &root().join("workspace"),
            Some(root().to_str().unwrap().to_string()),
            "test-user".to_string(),
        )
        .unwrap();

        let workspace_file = WorkspaceFile::builder()
            .workspace(
                WorkspaceConfig::builder()
                    .name("test-workspace".to_string())
                    .build()
                    .unwrap(),
            )
            .build()
            .unwrap();

        let workspace = Workspace::builder()
            .current_user("test-user".to_string())
            .paths(paths)
            .from_file(workspace_file)
            .await
            .unwrap()
            .build()
            .unwrap();

        LocalStore::new(&workspace)
    }
}
