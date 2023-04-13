use super::public::PublicArtifactDownload;
use super::{
    ArtifactId, ArtifactManifest, LocalStore, LocalStoreError, PackageManifest, PublicStore,
    PublicStoreError, Store, StoreError, MANIFEST_FILE,
};
use crate::archive::ArchiveManager;
use crate::model::ExecutableSpec;
use crate::sync::*;
use crate::Config;
use async_trait::async_trait;
use std::path::PathBuf;
use tracing::instrument;
use url::Url;

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

    #[instrument(name = "DefaultStore::download_from_public_store", skip(self))]
    async fn download_from_public_store(&self, key: ArtifactId) -> Result<(), StoreError> {
        if self.local_store.get_manifest(key.clone()).await?.is_some() {
            return Ok(());
        }

        if let PublicArtifactDownload::Downloaded = self.public_store.try_fetch(&key).await? {
            return Ok(());
        }

        Err(StoreError::CouldNotFindArtifactInPublicStore { key })
    }
}

#[async_trait]
impl Store for DefaultStore {
    /// Installs packages from the store via a Manifest Url.
    ///
    #[instrument(name = "DefaultStore::install_from_manifest_url", skip(self))]
    async fn install_from_manifest_url(&self, url: &Url) -> Result<ArtifactManifest, StoreError> {
        let manifest_path = if let Some(local_path) = self.config.public_store_metadata_path() {
            let path = url
                .path_segments()
                .unwrap()
                .collect::<Vec<&str>>()
                .join("/");
            local_path.join(path)
        } else {
            let archive = self.archive_manager.download(url).await?;
            archive.final_path().to_path_buf()
        };

        let package_manifest = PackageManifest::from_file(&manifest_path)
            .await
            .map_err(|err| StoreError::PackageManifestReadError {
                err,
                url: url.clone().into(),
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

    async fn find(&self, spec: &ExecutableSpec) -> Result<Option<ArtifactManifest>, StoreError> {
        match self.local_store.get_manifest(spec.artifact_id()).await? {
            None if spec.target().original_target().is_remote() => {
                // NB(@ostera): only remote targets will be available in the public store. Other
                // local targets may also be available in a `cloud_store` that is private per
                // organization, but we haven't included this in the Mark II yet.
                let _ = self.public_store.try_fetch(&spec.artifact_id()).await?;
                Ok(self.local_store.get_manifest(spec.artifact_id()).await?)
            }
            manifest => Ok(manifest),
        }
    }

    async fn clean(&self, spec: &ExecutableSpec) -> Result<(), StoreError> {
        self.local_store.clean(spec).await?;
        Ok(())
    }

    async fn promote(&self, am: &ArtifactManifest) -> Result<(), StoreError> {
        self.local_store.promote(am).await?;
        Ok(())
    }

    async fn save(
        &self,
        _spec: &ExecutableSpec,
        manifest: &ArtifactManifest,
    ) -> Result<(), StoreError> {
        let mut artifacts = manifest.outs().to_vec();
        artifacts.push(PathBuf::from(MANIFEST_FILE));

        self.local_store.write_manifest(manifest).await?;
        // self.remote_store.save(manifest, artifacts).await?;

        Ok(())
    }

    fn get_local_store_path_for_spec(&self, spec: &ExecutableSpec) -> PathBuf {
        let key = ArtifactId::new(spec.hash());
        self.local_store.get_absolute_path(key)
    }

    fn get_local_store_path_for_manifest(&self, am: &ArtifactManifest) -> PathBuf {
        self.local_store.get_absolute_path(am.id())
    }

    fn canonicalize_provided_artifact<N: AsRef<str>>(
        &self,
        am: &ArtifactManifest,
        name: N,
    ) -> Option<PathBuf> {
        let manifest_root = self.get_local_store_path_for_manifest(am);
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

        let mut server = mockito::Server::new_async().await;
        let mock_url = server.url().parse::<Url>().unwrap();

        let config = Config::builder()
            .warp_root(warp_root.path().to_path_buf())
            .public_store_cdn_url(mock_url.clone())
            .build()
            .unwrap();

        // NOTE(@ostera): this mock will be used by the public store to fetch an actual artifact
        let _public_store_mock1 = server
            .mock("GET", "/a-hash.tar.gz")
            .with_status(200)
            .with_body(include_bytes!("./fixtures/sample_artifact.tar.gz"))
            .create_async()
            .await;

        let _public_store_mock2 = server
            .mock("GET", "/b-hash.tar.gz")
            .with_status(200)
            .with_body(include_bytes!("./fixtures/sample_dependency.tar.gz"))
            .create_async()
            .await;

        // NOTE(@ostera): this mock will be used to download the manifest
        let _package_manifest_mock = server
            .mock("GET", "/sample/project/manifest.json")
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
            .create_async()
            .await;

        let am = ArchiveManager::new(&config).into();
        let ds = DefaultStore::new(config, am);

        let manifest_url = format!("{}sample/project/manifest.json", mock_url)
            .parse()
            .unwrap();
        let manifest = ds.install_from_manifest_url(&manifest_url).await.unwrap();

        assert_eq!(
            manifest.target(),
            "https://rules.warp.build/toolchains/erlang".to_string()
        );
        assert!(warp_root.child("store/a-hash/Manifest.json").exists());
        assert!(warp_root.child("store/b-hash/Manifest.json").exists());
    }

    #[tokio::test]
    async fn fails_when_public_store_cannot_download_artifact() {
        let warp_root = assert_fs::TempDir::new().unwrap();
        // NOTE(@ostera): this line is useful for debugging the output directory when something
        // goes wrong.
        // let store_root = store_root.into_persistent();

        let mut server = mockito::Server::new_async().await;
        let mock_url = server.url().parse::<Url>().unwrap();

        let config = Config::builder()
            .warp_root(warp_root.path().to_path_buf())
            .public_store_cdn_url(mock_url.clone())
            .build()
            .unwrap();

        // NOTE(@ostera): this mock will be used by the public store to fetch an actual artifact
        let _public_store_mock1 = server
            .mock("GET", "/a-hash.tar.gz")
            .with_status(200)
            .with_body(include_bytes!("./fixtures/sample_artifact.tar.gz"))
            .create_async()
            .await;

        // NOTE(@ostera): this mock will be used to download the manifest
        let _package_manifest_mock = server
            .mock("GET", "/sample/project/manifest.json")
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
            .create_async()
            .await;

        let am = ArchiveManager::new(&config).into();
        let ds = DefaultStore::new(config, am);

        let manifest_url = format!("{}sample/project/manifest.json", mock_url)
            .parse()
            .unwrap();
        let result = ds.install_from_manifest_url(&manifest_url).await;

        assert_matches!(
            result.unwrap_err(),
            StoreError::CouldNotFindArtifactInPublicStore{
                key
            } => {
                assert_eq!(key, ArtifactId::new("b-hash"));
            }
        );
    }

    #[tokio::test]
    async fn fails_when_we_cannot_find_artifact_keys_to_download() {
        let warp_root = assert_fs::TempDir::new().unwrap();
        // NOTE(@ostera): this line is useful for debugging the output directory when something
        // goes wrong.
        // let store_root = store_root.into_persistent();

        let mut server = mockito::Server::new_async().await;
        let mock_url = server.url().parse::<Url>().unwrap();

        let config = Config::builder()
            .warp_root(warp_root.path().to_path_buf())
            .public_store_cdn_url(mock_url.clone())
            .build()
            .unwrap();

        // NOTE(@ostera): this mock will be used to download the manifest
        let _package_manifest_mock = server
            .mock("GET", "/sample/project/manifest.json")
            .with_status(200)
            .with_body(
                r#"
{
    "published_at": "2023-03-01T21:09:32+00:00",
    "keys": {}
}
                "#,
            )
            .create_async()
            .await;

        let am = ArchiveManager::new(&config).into();
        let ds = DefaultStore::new(config, am);

        let manifest_url = format!("{}sample/project/manifest.json", mock_url)
            .parse()
            .unwrap();
        let result = ds.install_from_manifest_url(&manifest_url).await;

        assert_matches!(
            result.unwrap_err(),
            StoreError::CouldNotFindHostTripleInManifest { .. }
        );
    }
}
