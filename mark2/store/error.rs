use super::ManifestUrl;
use super::PublicStoreError;
use crate::archive::ArchiveManagerError;
use crate::util::from_file::FromFileError;
use thiserror::Error;
use url::Url;

#[derive(Debug, Error)]
pub enum StoreError {
    #[error("Something went wrong with the store")]
    Unknown,

    #[error("Tried to install an artifact by url, but the url {url:?} is not installable.")]
    UrlIsNotInstallable { url: Url },

    #[error("Could not read the package manifest at {url:?} due to: {err:?}")]
    PackageManifestReadError {
        err: FromFileError,
        url: ManifestUrl,
    },

    #[error(transparent)]
    ArchiveManagerError(ArchiveManagerError),

    #[error(transparent)]
    PublicStoreError(PublicStoreError),

    #[error(
        "Could not find the current host architecture in the package manifest to be installed"
    )]
    CouldNotFindHostTripleInManifest { host: String },
}

impl From<ArchiveManagerError> for StoreError {
    fn from(value: ArchiveManagerError) -> Self {
        StoreError::ArchiveManagerError(value)
    }
}
