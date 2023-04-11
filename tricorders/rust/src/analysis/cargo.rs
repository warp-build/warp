use super::model::{Signature, SignatureError};
use crate::dependencies::DependencyManager;
use cargo_toml::{Manifest, Product};
use std::path::Path;
use std::sync::Arc;
use thiserror::Error;

#[derive(Default)]
pub struct CargoAnalyzer {
    dep_manager: Arc<DependencyManager>,
}

impl CargoAnalyzer {
    pub fn new(dep_manager: Arc<DependencyManager>) -> Self {
        Self { dep_manager }
    }

    pub async fn generate_signature(
        &self,
        workspace_root: &Path,
        file: &Path,
    ) -> Result<Vec<Signature>, CargoAnalyzerError> {
        let signatures = vec![];
        let manifest = Manifest::from_path(workspace_root.join(file))?;
        Ok(signatures)
    }

    fn cargo_bin_signature(&self, bin: &Product) -> Result<Signature, CargoAnalyzerError> {
        let sig = Signature::builder().build()?;
        Ok(sig)
    }
}

#[derive(Error, Debug)]
pub enum CargoAnalyzerError {
    #[error(transparent)]
    BadManifest(cargo_toml::Error),

    #[error(transparent)]
    SignatureBuildingError(SignatureError),
}

impl From<cargo_toml::Error> for CargoAnalyzerError {
    fn from(value: cargo_toml::Error) -> Self {
        CargoAnalyzerError::BadManifest(value)
    }
}

impl From<SignatureError> for CargoAnalyzerError {
    fn from(value: SignatureError) -> Self {
        CargoAnalyzerError::SignatureBuildingError(value)
    }
}
