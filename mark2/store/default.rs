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
            .unwrap()
        {
            let key = ArtifactId::new(key);
            downloads.push(self.download_from_public_store(key))
        }

        let _results = futures::future::join_all(downloads).await;

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
}
