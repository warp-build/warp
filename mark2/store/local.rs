use super::{ArtifactId, ArtifactManifest, ARTIFACT_MANIFEST_FILE};
use crate::{util::from_file::FromFileError, Config};
use std::path::PathBuf;
use thiserror::Error;

#[derive(Debug, Clone)]
pub struct LocalStore {
    config: Config,
    store_root: PathBuf,
}

impl LocalStore {
    pub fn new(config: Config) -> Self {
        Self {
            store_root: config.artifact_store_root().to_path_buf(),
            config,
        }
    }

    pub fn get_absolute_path(&self, key: ArtifactId) -> PathBuf {
        self.store_root.join(key.inner())
    }

    pub async fn get_manifest(
        &self,
        key: ArtifactId,
    ) -> Result<Option<ArtifactManifest>, LocalStoreError> {
        let artifact_path = self
            .store_root
            .join(key.inner())
            .join(ARTIFACT_MANIFEST_FILE);

        match ArtifactManifest::from_file(&artifact_path).await {
            Ok(manifest) => Ok(Some(manifest)),
            Err(FromFileError::CouldNotOpenFile { .. }) => Ok(None),
            Err(err) => Err(LocalStoreError::CouldNotReadManifest(err)),
        }
    }
}

#[derive(Error, Debug)]
pub enum LocalStoreError {
    #[error(transparent)]
    IoError(std::io::Error),

    #[error(transparent)]
    CouldNotReadManifest(FromFileError),
}

#[cfg(test)]
mod tests {
    use super::*;
    use assert_fs::prelude::*;

    #[tokio::test]
    async fn finds_and_reads_manifest() {
        let store_root = assert_fs::TempDir::new().unwrap();

        let config = Config::builder()
            .artifact_store_root(store_root.path().to_path_buf())
            .build()
            .unwrap();

        let ls = LocalStore::new(config);

        let manifest = store_root.child("a-hash/Manifest.json");
        manifest
            .write_str(include_str!("./fixtures/Manifest.json"))
            .unwrap();

        let key = ArtifactId::new("a-hash");
        let manifest = ls.get_manifest(key).await.unwrap().unwrap();

        assert_eq!(
            manifest.target(),
            "https://rules.warp.build/toolchains/erlang".to_string()
        );
    }

    #[tokio::test]
    async fn does_not_error_on_missing_artifacts() {
        let store_root = assert_fs::TempDir::new().unwrap();

        let config = Config::builder()
            .artifact_store_root(store_root.path().to_path_buf())
            .build()
            .unwrap();

        let ls = LocalStore::new(config);

        let key = ArtifactId::new("a-hash");
        assert!(ls.get_manifest(key).await.unwrap().is_none());
    }

    #[tokio::test]
    async fn fails_on_bad_manifests() {
        let store_root = assert_fs::TempDir::new().unwrap();

        let config = Config::builder()
            .artifact_store_root(store_root.path().to_path_buf())
            .build()
            .unwrap();

        let ls = LocalStore::new(config);

        let manifest = store_root.child("a-hash/Manifest.json");
        manifest.write_str("garbage").unwrap();

        let key = ArtifactId::new("a-hash");
        let result = ls.get_manifest(key).await;
        assert!(result.is_err());
        assert_matches!(
            result.unwrap_err(),
            LocalStoreError::CouldNotReadManifest(_)
        );
    }
}
