use super::model::{Config, Requirement, Signature, SignatureError};
use crate::dependencies::cargo_manifest::{
    Binary, CargoManifest, CargoManifestError, Library, Package,
};
use crate::dependencies::crates::{Crates, CratesError};
use crate::dependencies::DependencyManager;
use crate::RUST_TRICORDER_URL;

use std::path::{Path, PathBuf};
use std::sync::Arc;
use thiserror::Error;

pub const RULE_RUST_BINARY: &str = "rust_binary";
pub const RULE_RUST_LIBRARY: &str = "rust_library";

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
        println!("Generating signature for {file:?}");
        let mut signatures = vec![];
        let manifest_path = workspace_root.join(file);
        let manifest = CargoManifest::from_path(&manifest_path)?;
        let Some(package) = manifest.package() else { return Ok(vec![]) };

        println!("Collecting requirements for {file:?}");
        let deps = self.collect_requirements(package).await?;
        println!("Found {} requirements", deps.len());

        let manifest_root = manifest_path.parent().unwrap();

        for bin in package.bin() {
            signatures.push(self.cargo_bin_signature(package, manifest_root, bin, &deps)?);
        }

        for lib in package.lib() {
            signatures.push(self.cargo_lib_signature(package, manifest_root, lib, &deps)?);
        }

        Ok(signatures)
    }

    async fn collect_requirements(
        &self,
        manifest: &Package,
    ) -> Result<Vec<Requirement>, CargoAnalyzerError> {
        let mut reqs = vec![];

        'dep_loop: for dep in manifest
            .dependencies()
            .iter()
            .chain(manifest.build_dependencies().iter())
        {
            if dep.is_crates_io() {
                let pkgs = self
                    .dep_manager
                    .dependencies()
                    .get(dep.name())
                    .map(|r| r.clone())
                    .unwrap_or_default();

                if let Some(pkg) = pkgs
                    .iter()
                    .find(|pkg| dep.version_req().matches(pkg.version()))
                {
                    println!(
                        "Found dep {}-{} matching {}",
                        dep.name(),
                        pkg.version(),
                        dep.version_req().to_string()
                    );

                    let url = self
                        .crates
                        .download_url(dep.name(), &pkg.version().to_string())
                        .await?;

                    let subpath = PathBuf::from(format!("{}-{}", dep.name(), pkg.version()));

                    let req = Requirement::Remote {
                        name: dep.name().to_string(),
                        url,
                        subpath,
                        tricorder: RUST_TRICORDER_URL.clone(),
                    };
                    reqs.push(req);
                    continue 'dep_loop;
                }
                println!("Dep {} not found", dep.name());
            }
        }

        Ok(reqs)
    }

    fn cargo_bin_signature(
        &self,
        package: &Package,
        manifest_root: &Path,
        bin: &Binary,
        deps: &[Requirement],
    ) -> Result<Signature, CargoAnalyzerError> {
        let target = bin.name().to_string();

        let main = bin
            .src_path()
            .strip_prefix(manifest_root)
            .unwrap()
            .to_path_buf();

        let config = Config::builder()
            .insert("cargo_name", bin.name().to_string())
            .insert("crate_name", bin.name().to_string().replace('-', "_"))
            .insert("edition", package.edition().to_string())
            .insert("srcs", self.list_files(manifest_root)?)
            .insert("main", main)
            .build();

        let sig = Signature::builder()
            .rule(RULE_RUST_BINARY)
            .name(target)
            .deps(deps.to_vec())
            .config(config)
            .build()?;

        Ok(sig)
    }

    fn cargo_lib_signature(
        &self,
        package: &Package,
        manifest_root: &Path,
        lib: &Library,
        deps: &[Requirement],
    ) -> Result<Signature, CargoAnalyzerError> {
        let target = lib.name().to_string();

        let main = lib
            .src_path()
            .strip_prefix(manifest_root)
            .unwrap()
            .to_path_buf();

        let config = Config::builder()
            .insert("edition", package.edition().to_string())
            .insert("cargo_name", lib.name().to_string())
            .insert("crate_name", lib.name().to_string().replace('-', "_"))
            .insert("crate_type", lib.crate_type().to_string())
            .insert("srcs", self.list_files(manifest_root)?)
            .insert("main", main)
            .build();

        let sig = Signature::builder()
            .rule(RULE_RUST_LIBRARY)
            .name(target)
            .deps(deps.to_vec())
            .config(config)
            .build()?;

        Ok(sig)
    }

    fn list_files(&self, root: &Path) -> Result<Vec<PathBuf>, ListFilesError> {
        let files: Vec<PathBuf> = ignore::WalkBuilder::new(root)
            .git_ignore(false)
            .git_global(false)
            .git_exclude(false)
            .build()
            .map(|entry| entry.map(|e| e.into_path().strip_prefix(root).unwrap().to_path_buf()))
            .collect::<Result<_, ignore::Error>>()?;

        Ok(files)
    }
}

#[derive(Error, Debug)]
pub enum ListFilesError {
    #[error(transparent)]
    Walk(ignore::Error),
}

impl From<ignore::Error> for ListFilesError {
    fn from(value: ignore::Error) -> Self {
        ListFilesError::Walk(value)
    }
}

#[derive(Error, Debug)]
pub enum CargoAnalyzerError {
    #[error(transparent)]
    SignatureBuildingError(SignatureError),

    #[error(transparent)]
    CratesError(CratesError),

    #[error(transparent)]
    CargoManifestError(CargoManifestError),

    #[error(transparent)]
    ListFiles(ListFilesError),
}

impl From<ListFilesError> for CargoAnalyzerError {
    fn from(value: ListFilesError) -> Self {
        CargoAnalyzerError::ListFiles(value)
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

impl From<CargoManifestError> for CargoAnalyzerError {
    fn from(value: CargoManifestError) -> Self {
        CargoAnalyzerError::CargoManifestError(value)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use assert_fs::prelude::*;

    #[tokio::test]
    async fn generates_bin_signatures() {
        let workspace_root = assert_fs::TempDir::new().unwrap();
        dbg!(&workspace_root);

        let cargo_toml = workspace_root.child("Cargo.toml");
        cargo_toml
            .write_str(
                r#"
[package]
name = "test"
version = "0.1.0"

[[bin]]
name = "test"
path = "src/main.rs"

[dependencies]
anyhow = "1"
            "#,
            )
            .unwrap();

        workspace_root
            .child("Cargo.lock")
            .write_str(
                r#"
# This file is automatically @generated by Cargo.
# It is not intended for manual editing.
version = 3

[[package]]
name = "anyhow"
version = "1.0.68"
source = "registry+https://github.com/rust-lang/crates.io-index"
checksum = "2cb2f989d18dd141ab8ae82f64d1a8cdd37e0840f73a406896cf5e99502fab61"
            "#,
            )
            .unwrap();

        let crates = Arc::new(Crates::new());
        let dep_manager = Arc::new(DependencyManager::new(workspace_root.path()));
        dep_manager.prepare().await.unwrap();

        let cargo_analyzer = CargoAnalyzer::new(crates, dep_manager);

        let signatures = cargo_analyzer
            .generate_signature(workspace_root.path(), cargo_toml.path())
            .await
            .unwrap();

        assert_eq!(signatures.len(), 1);

        let test_bin = signatures.get(0).unwrap();

        assert_eq!(test_bin.name(), "test");
        assert_eq!(test_bin.rule(), RULE_RUST_BINARY);

        assert_eq!(test_bin.deps().len(), 1);
        let dep = test_bin.deps().get(0).unwrap();

        dbg!(&dep);
        assert_matches!(dep, Requirement::Remote { url, subpath, .. } => {
            assert_eq!(url.to_string(), "https://crates.io/api/v1/crates/anyhow/1.0.68/download".to_string());
            assert_eq!(*subpath, PathBuf::from("anyhow-1.0.68"));
        });
    }

    #[tokio::test]
    async fn generates_lib_signatures() {
        let workspace_root = assert_fs::TempDir::new().unwrap();
        dbg!(&workspace_root);

        let cargo_toml = workspace_root.child("Cargo.toml");
        cargo_toml
            .write_str(
                r#"
[package]
name = "test"
version = "0.1.0"

[lib]
path = "src/lib.rs"

[dependencies]
anyhow = "1"
            "#,
            )
            .unwrap();

        workspace_root
            .child("Cargo.lock")
            .write_str(
                r#"
# This file is automatically @generated by Cargo.
# It is not intended for manual editing.
version = 3

[[package]]
name = "anyhow"
version = "1.0.68"
source = "registry+https://github.com/rust-lang/crates.io-index"
checksum = "2cb2f989d18dd141ab8ae82f64d1a8cdd37e0840f73a406896cf5e99502fab61"
            "#,
            )
            .unwrap();

        let crates = Arc::new(Crates::new());
        let dep_manager = Arc::new(DependencyManager::new(workspace_root.path()));
        dep_manager.prepare().await.unwrap();

        let cargo_analyzer = CargoAnalyzer::new(crates, dep_manager);

        let signatures = cargo_analyzer
            .generate_signature(workspace_root.path(), cargo_toml.path())
            .await
            .unwrap();

        assert_eq!(signatures.len(), 1);

        let test_lib = signatures.get(0).unwrap();

        assert_eq!(test_lib.name(), "test");
        assert_eq!(test_lib.rule(), RULE_RUST_LIBRARY);

        assert_eq!(test_lib.deps().len(), 1);
        let dep = test_lib.deps().get(0).unwrap();

        dbg!(&dep);
        assert_matches!(dep, Requirement::Remote { url, subpath, .. } => {
            assert_eq!(url.to_string(), "https://crates.io/api/v1/crates/anyhow/1.0.68/download".to_string());
            assert_eq!(*subpath, PathBuf::from("anyhow-1.0.68"));
        });
    }
}
