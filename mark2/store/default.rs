use std::path::PathBuf;

use super::{
    ArtifactId, ArtifactManifest, LocalStore, LocalStoreError, ManifestUrl, PackageManifest,
    PublicStore, PublicStoreError, Store, StoreError,
};
use crate::archive::ArchiveManager;
use crate::sync::*;
use crate::Config;
use async_trait::async_trait;

/// The Default store implements the interface of an artifact store that orchestrates between the
/// Public and Local stores to download, cache, and save artifacts.
///
#[derive(Debug, Clone)]
pub struct DefaultStore {
    config: Config,
    archive_manager: Arc<ArchiveManager>,
    public_store: PublicStore,
    local_store: LocalStore,
}

impl DefaultStore {
    pub fn new(config: Config, archive_manager: Arc<ArchiveManager>) -> Self {
        Self {
            public_store: PublicStore::new(config.clone()),
            local_store: LocalStore::new(config.clone()),
            config,
            archive_manager,
        }
    }

    async fn download_from_public_store(&self, key: ArtifactId) -> Result<(), StoreError> {
        self.public_store.try_fetch(&key).await?;
        Ok(())
    }
}

#[async_trait]
impl Store for DefaultStore {
    /// Installs packages from the store via a Manifest Url.
    ///
    async fn install_from_manifest_url(
        &self,
        url: &ManifestUrl,
    ) -> Result<ArtifactManifest, StoreError> {
        let archive = self.archive_manager.download(url.url()).await?;

        let package_manifest = PackageManifest::from_file(archive.final_path())
            .await
            .map_err(|err| StoreError::PackageManifestReadError {
                err,
                url: url.clone(),
            })?;

        let mut downloads = vec![];
        let artifact_keys = package_manifest
            .keys()
            .get(self.config.host_env().host_triple())
            .ok_or_else(|| StoreError::CouldNotFindHostTripleInManifest {
                host: self.config.host_env().host_triple().to_string(),
            })?;

        for key in artifact_keys {
            let key = ArtifactId::new(key);
            downloads.push(self.download_from_public_store(key))
        }

        for result in futures::future::join_all(downloads).await {
            result?;
        }

        let main_artifact = ArtifactId::new(artifact_keys.get(0).unwrap());
        let manifest = self.local_store.get_manifest(main_artifact).await?;
        Ok(manifest.unwrap())
    }

    fn canonicalize_provided_artifact<N: AsRef<str>>(
        &self,
        am: &ArtifactManifest,
        name: N,
    ) -> Option<PathBuf> {
        let manifest_root = self.local_store.get_absolute_path(am.id());
        am.provides(name.as_ref())
            .map(|rel_path| manifest_root.join(rel_path))
    }
}

impl From<PublicStoreError> for StoreError {
    fn from(value: PublicStoreError) -> Self {
        StoreError::PublicStoreError(value)
    }
}

impl From<LocalStoreError> for StoreError {
    fn from(value: LocalStoreError) -> Self {
        StoreError::LocalStoreError(value)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use assert_fs::prelude::*;

    #[tokio::test]
    async fn can_install_from_valid_manifest_url() {
        let warp_root = assert_fs::TempDir::new().unwrap();
        // NOTE(@ostera): this line is useful for debugging the output directory when something
        // goes wrong.
        // let store_root = store_root.into_persistent();

        let config = Config::builder()
            .warp_root(warp_root.path().to_path_buf())
            .public_store_cdn_url(mockito::server_url().parse().unwrap())
            .build()
            .unwrap();

        // NOTE(@ostera): this mock will be used by the public store to fetch an actual artifact
        let _public_store_mock1 = mockito::mock("GET", "/a-hash.tar.gz")
            .with_status(200)
            .with_body(include_bytes!("./fixtures/sample_artifact.tar.gz"))
            .create();

        let _public_store_mock2 = mockito::mock("GET", "/b-hash.tar.gz")
            .with_status(200)
            .with_body(include_bytes!("./fixtures/sample_dependency.tar.gz"))
            .create();

        // NOTE(@ostera): this mock will be used to download the manifest
        let _package_manifest_mock = mockito::mock("GET", "/sample/project/manifest.json")
            .with_status(200)
            .with_body(
                r#"
{
    "published_at": "2023-03-01T21:09:32+00:00",
    "keys": {
        "aarch64-apple-darwin": [ "a-hash", "b-hash" ],
        "x86_64-apple-darwin": [ "a-hash", "b-hash" ],
        "aarch64-unknown-linux-gnu": [ "a-hash", "b-hash" ],
        "x86_64-unknown-linux-gnu": [ "a-hash", "b-hash" ]
    }
}
                "#,
            )
            .create();

        let am = ArchiveManager::new(&config).into();
        let ds = DefaultStore::new(config, am);

        let manifest_url = ManifestUrl::new(
            format!("{}/sample/project/manifest.json", mockito::server_url())
                .parse()
                .unwrap(),
        );
        let manifest = ds.install_from_manifest_url(&manifest_url).await.unwrap();

        assert_eq!(
            manifest.target(),
            "https://rules.warp.build/toolchains/erlang".to_string()
        );
        assert!(warp_root
            .child("artifact_store/a-hash/Manifest.json")
            .exists());
        assert!(warp_root
            .child("artifact_store/b-hash/Manifest.json")
            .exists());
    }

    #[tokio::test]
    async fn fails_when_public_store_cannot_download_artifact() {
        let warp_root = assert_fs::TempDir::new().unwrap();
        // NOTE(@ostera): this line is useful for debugging the output directory when something
        // goes wrong.
        // let store_root = store_root.into_persistent();

        let config = Config::builder()
            .warp_root(warp_root.path().to_path_buf())
            .public_store_cdn_url(mockito::server_url().parse().unwrap())
            .build()
            .unwrap();

        // NOTE(@ostera): this mock will be used by the public store to fetch an actual artifact
        let _public_store_mock1 = mockito::mock("GET", "/a-hash.tar.gz")
            .with_status(200)
            .with_body(include_bytes!("./fixtures/sample_artifact.tar.gz"))
            .create();

        // NOTE(@ostera): this mock will be used to download the manifest
        let _package_manifest_mock = mockito::mock("GET", "/sample/project/manifest.json")
            .with_status(200)
            .with_body(
                r#"
{
    "published_at": "2023-03-01T21:09:32+00:00",
    "keys": {
        "aarch64-apple-darwin": [ "a-hash", "b-hash" ],
        "x86_64-apple-darwin": [ "a-hash", "b-hash" ],
        "aarch64-unknown-linux-gnu": [ "a-hash", "b-hash" ],
        "x86_64-unknown-linux-gnu": [ "a-hash", "b-hash" ]
    }
}
                "#,
            )
            .create();

        let am = ArchiveManager::new(&config).into();
        let ds = DefaultStore::new(config, am);

        let manifest_url = ManifestUrl::new(
            format!("{}/sample/project/manifest.json", mockito::server_url())
                .parse()
                .unwrap(),
        );
        let result = ds.install_from_manifest_url(&manifest_url).await;

        assert_matches!(
            result.unwrap_err(),
            StoreError::PublicStoreError(PublicStoreError::CouldNotDownloadArtifact {
                id
            }) if id.inner() == "b-hash"
        );
    }

    #[tokio::test]
    async fn fails_when_we_cannot_find_artifact_keys_to_download() {
        let warp_root = assert_fs::TempDir::new().unwrap();
        // NOTE(@ostera): this line is useful for debugging the output directory when something
        // goes wrong.
        // let store_root = store_root.into_persistent();

        let config = Config::builder()
            .warp_root(warp_root.path().to_path_buf())
            .public_store_cdn_url(mockito::server_url().parse().unwrap())
            .build()
            .unwrap();

        // NOTE(@ostera): this mock will be used to download the manifest
        let _package_manifest_mock = mockito::mock("GET", "/sample/project/manifest.json")
            .with_status(200)
            .with_body(
                r#"
{
    "published_at": "2023-03-01T21:09:32+00:00",
    "keys": {}
}
                "#,
            )
            .create();

        let am = ArchiveManager::new(&config).into();
        let ds = DefaultStore::new(config, am);

        let manifest_url = ManifestUrl::new(
            format!("{}/sample/project/manifest.json", mockito::server_url())
                .parse()
                .unwrap(),
        );
        let result = ds.install_from_manifest_url(&manifest_url).await;

        assert_matches!(
            result.unwrap_err(),
            StoreError::CouldNotFindHostTripleInManifest { .. }
        );
    }
}
