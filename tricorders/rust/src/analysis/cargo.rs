use super::model::{Requirement, Signature, SignatureError};
use crate::dependencies::crates::{Crates, CratesError};
use crate::dependencies::DependencyManager;
use crate::RUST_TRICORDER_URL;
use cargo_toml::{Manifest, Product};
use std::path::{Path, PathBuf};
use std::sync::Arc;
use thiserror::Error;

pub struct CargoAnalyzer {
    crates: Arc<Crates>,
    dep_manager: Arc<DependencyManager>,
}

impl CargoAnalyzer {
    pub fn new(crates: Arc<Crates>, dep_manager: Arc<DependencyManager>) -> Self {
        Self {
            crates,
            dep_manager,
        }
    }

    pub async fn generate_signature(
        &self,
        workspace_root: &Path,
        file: &Path,
    ) -> Result<Vec<Signature>, CargoAnalyzerError> {
        let mut signatures = vec![];
        let manifest = Manifest::from_path(workspace_root.join(file))?;

        let deps = self.collect_requirements(&manifest).await?;

        for bin in &manifest.bin {
            signatures.push(self.cargo_bin_signature(bin, &deps)?);
        }

        for lib in &manifest.lib {
            signatures.push(self.cargo_lib_signature(lib, &deps)?);
        }

        Ok(signatures)
    }

    async fn collect_requirements(
        &self,
        manifest: &Manifest,
    ) -> Result<Vec<Requirement>, CargoAnalyzerError> {
        let mut reqs = vec![];

        for (name, dep) in manifest
            .dev_dependencies
            .iter()
            .chain(manifest.dependencies.iter())
        {
            if dep.is_crates_io() {
                if let Some(pkg) = self.dep_manager.dependencies().get(name) {
                    let url = self.crates.download_url(name, pkg.version()).await?;
                    let subpath = Some(PathBuf::from(format!("{}-{}.crate", name, pkg.version())));
                    let req = Requirement::Url {
                        url,
                        subpath,
                        tricorder_url: RUST_TRICORDER_URL.clone(),
                    };
                    reqs.push(req);
                }
            }
        }

        Ok(reqs)
    }

    fn cargo_bin_signature(
        &self,
        _bin: &Product,
        _deps: &[Requirement],
    ) -> Result<Signature, CargoAnalyzerError> {
        let sig = Signature::builder().build()?;
        Ok(sig)
    }

    fn cargo_lib_signature(
        &self,
        _lib: &Product,
        _deps: &[Requirement],
    ) -> Result<Signature, CargoAnalyzerError> {
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

    #[error(transparent)]
    CratesError(CratesError),
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

impl From<CratesError> for CargoAnalyzerError {
    fn from(value: CratesError) -> Self {
        CargoAnalyzerError::CratesError(value)
    }
}
