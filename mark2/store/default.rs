use super::public_store::PublicStoreError;
use super::{ArtifactId, ManifestUrl, PackageManifest, PublicStore, Store, StoreError};
use crate::archive::ArchiveManager;
use crate::sync::*;
use crate::Config;
use async_trait::async_trait;

pub struct DefaultStore {
    config: Config,
    archive_manager: Arc<ArchiveManager>,
    public_store: PublicStore,
}

impl DefaultStore {
    pub fn new(config: Config, archive_manager: Arc<ArchiveManager>) -> Self {
        Self {
            public_store: PublicStore::new(config.clone()),
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
    async fn install_from_manifest_url(&self, url: &ManifestUrl) -> Result<(), StoreError> {
        let archive = self.archive_manager.download(url.url()).await?;

        let package_manifest = PackageManifest::from_file(archive.final_path())
            .await
            .map_err(|err| StoreError::PackageManifestReadError {
                err,
                url: url.clone(),
            })?;

        let mut downloads = vec![];
        for key in package_manifest
            .keys()
            .get(self.config.host_env().host_triple())
            .ok_or_else(|| StoreError::InvalidHostTripleInManifest {
                host: self.config.host_env().host_triple().to_string(),
            })?
        {
            let key = ArtifactId::new(key);
            downloads.push(self.download_from_public_store(key))
        }

        for result in futures::future::join_all(downloads).await {
            result?;
        }

        Ok(())
    }
}

impl From<PublicStoreError> for StoreError {
    fn from(value: PublicStoreError) -> Self {
        StoreError::PublicStoreError(value)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use assert_fs::prelude::*;
    use url::Url;

    #[tokio::test]
    async fn can_install_from_valid_manifest_url() {
        let store_root = assert_fs::TempDir::new().unwrap();
        // NOTE(@ostera): this line is useful for debugging the output directory when something
        // goes wrong.
        // let store_root = store_root.into_persistent();

        let config = Config::builder()
            .store_root(store_root.path().to_path_buf())
            .public_store_url(mockito::server_url().parse().unwrap())
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
        let _package_manifest_mock = mockito::mock("GET", "/manifest.json")
            .with_status(200)
            .with_body(
                r#"
{
    "published_at": "2023-03-01T21:09:32+00:00",
    "keys": {
        "aarch64-apple-darwin": [ "a-hash", "b-hash" ],
        "x86_64-apple-darwin": [ "a-hash", "b-hash" ],
        "aarch64-unknown-linux": [ "a-hash", "b-hash" ],
        "x86_64-unknown-linux": [ "a-hash", "b-hash" ]
    }
}
                "#,
            )
            .create();

        let am = ArchiveManager::new(&config).into();
        let ds = DefaultStore::new(config, am);

        let manifest_url = ManifestUrl::new(
            format!("{}/manifest.json", mockito::server_url())
                .parse()
                .unwrap(),
        );
        ds.install_from_manifest_url(&manifest_url).await.unwrap();

        dbg!(&store_root.path());
        assert!(store_root.child("a-hash/sample_file").exists());
        assert!(store_root.child("b-hash/sample_dependency").exists());
    }
}
