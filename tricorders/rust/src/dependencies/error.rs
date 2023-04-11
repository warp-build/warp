use super::cargo_lock::CargoLockError;
use super::cargo_manifest::CargoManifestError;
use super::rust_toolchain::RustToolchainError;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum DependencyManagerError {
    #[error(transparent)]
    RustToolchainError(RustToolchainError),

    #[error(transparent)]
    CargoLockError(CargoLockError),

    #[error(transparent)]
    FileWalkingError(ignore::Error),

    #[error(transparent)]
    IoError(std::io::Error),

    #[error(transparent)]
    CargoManifestError(CargoManifestError),
}

impl From<RustToolchainError> for DependencyManagerError {
    fn from(value: RustToolchainError) -> Self {
        DependencyManagerError::RustToolchainError(value)
    }
}

impl From<CargoLockError> for DependencyManagerError {
    fn from(value: CargoLockError) -> Self {
        DependencyManagerError::CargoLockError(value)
    }
}

impl From<ignore::Error> for DependencyManagerError {
    fn from(value: ignore::Error) -> Self {
        DependencyManagerError::FileWalkingError(value)
    }
}

impl From<std::io::Error> for DependencyManagerError {
    fn from(value: std::io::Error) -> Self {
        DependencyManagerError::IoError(value)
    }
}

impl From<CargoManifestError> for DependencyManagerError {
    fn from(value: CargoManifestError) -> Self {
        DependencyManagerError::CargoManifestError(value)
    }
}
