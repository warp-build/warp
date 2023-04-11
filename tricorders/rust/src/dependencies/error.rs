use super::cargo_lock::CargoLockError;
use super::rust_toolchain::RustToolchainError;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum DependencyManagerError {
    #[error(transparent)]
    CargoTomlError(cargo_toml::Error),

    #[error(transparent)]
    RustToolchainError(RustToolchainError),

    #[error(transparent)]
    CargoLockError(CargoLockError),
}

impl From<cargo_toml::Error> for DependencyManagerError {
    fn from(value: cargo_toml::Error) -> Self {
        DependencyManagerError::CargoTomlError(value)
    }
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
