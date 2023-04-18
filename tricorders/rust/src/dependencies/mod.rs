pub mod cargo_lock;
pub mod cargo_manifest;
pub mod crates;
mod error;
pub mod rust_toolchain;

pub use error::*;

use self::cargo_lock::{CargoLock, Package};
use self::cargo_manifest::CargoManifest;
use self::crates::Crates;
use self::rust_toolchain::RustToolchain;
use dashmap::DashMap;
use std::path::{Path, PathBuf};
use std::sync::{Arc, RwLock};
use tracing::{debug, info};

#[derive(Default, Clone, Eq, PartialEq)]
pub enum Status {
    #[default]
    Idle,
    Loading,
    Ready,
}

pub struct DependencyManager {
    status: Arc<RwLock<Status>>,
    workspace_root: PathBuf,
    cargo_locks: DashMap<PathBuf, CargoLock>,
    cargo_manifests: DashMap<PathBuf, CargoManifest>,
    rust_toolchain: Arc<RwLock<RustToolchain>>,
    crates: Arc<Crates>,
    dependencies: DashMap<String, Vec<Package>>,
}

impl DependencyManager {
    pub fn new(workspace_root: &Path) -> Self {
        let crates = Arc::new(Crates::new());
        Self {
            workspace_root: workspace_root.to_path_buf(),
            status: Default::default(),
            cargo_manifests: Default::default(),
            cargo_locks: Default::default(),
            rust_toolchain: Default::default(),
            dependencies: Default::default(),
            crates,
        }
    }

    pub async fn prepare(&self) -> Result<(), DependencyManagerError> {
        self.mark_as_loading();
        self.load_manifests().await?;
        self.consolidate_dependencies();
        self.mark_as_ready();
        Ok(())
    }

    async fn load_manifests(&self) -> Result<(), DependencyManagerError> {
        let root = self.workspace_root.clone();

        info!("Loading manifests from {:?}", &root);
        let mut loaded = 0;
        for result in ignore::Walk::new(&root) {
            let entry = result?;
            let path = root.join(entry.path());

            let Some(filename) = path.file_name() else { continue; };

            match filename.to_string_lossy().as_ref() {
                "Cargo.lock" => {
                    info!("Found Cargo.lock: {:?}", &path);
                    let cargo_manifest = CargoLock::from_path(&path).await?;
                    let path = path.strip_prefix(&root).unwrap().to_path_buf();
                    self.cargo_locks.insert(path, cargo_manifest);
                    loaded += 1;
                }
                "Cargo.toml" => {
                    info!("Found Cargo.toml: {:?}", &path);
                    let cargo_manifest = CargoManifest::from_path(&path)?;
                    let path = path.strip_prefix(&root).unwrap().to_path_buf();
                    self.cargo_manifests.insert(path, cargo_manifest);

                    loaded += 1;
                }
                "rust-toolchain.toml" => {
                    info!("Found rust-toolchain.toml: {:?}", &path);
                    let rust_toolchain = RustToolchain::from_path(&path).await?;
                    (*self.rust_toolchain.try_write().unwrap()) = rust_toolchain;
                    loaded += 1;
                }
                file => {
                    debug!("Ignored {:?}", file);
                }
            }
        }

        info!("Finished loading {} manifests from {:?}", loaded, &root);

        Ok(())
    }

    fn consolidate_dependencies(&self) {
        let deps: DashMap<String, Vec<Package>> = DashMap::default();
        for lockfile in self.cargo_locks() {
            for package in lockfile.packages() {
                let mut old_pkgs = deps
                    .get(package.name())
                    .map(|r| r.clone())
                    .unwrap_or_default();
                old_pkgs.push(package.clone());
                deps.insert(package.name().to_string(), old_pkgs);
            }
        }
        for (name, dep) in deps {
            self.dependencies.insert(name, dep);
        }
    }

    fn mark_as_loading(&self) {
        (*self.status.try_write().unwrap()) = Status::Loading;
    }

    fn mark_as_ready(&self) {
        (*self.status.try_write().unwrap()) = Status::Ready;
    }

    pub async fn wait_until_ready(&self) {
        loop {
            if (*self.status.try_read().unwrap()) == Status::Ready {
                return;
            }
            tokio::time::sleep(std::time::Duration::from_millis(10)).await;
        }
    }

    pub fn cargo_manifests(&self) -> DashMap<PathBuf, CargoManifest> {
        self.cargo_manifests.clone()
    }

    pub fn rust_toolchain(&self) -> RustToolchain {
        self.rust_toolchain.try_read().unwrap().clone()
    }

    pub fn cargo_locks(&self) -> &DashMap<PathBuf, CargoLock> {
        &self.cargo_locks
    }

    pub fn dependencies(&self) -> &DashMap<String, Vec<Package>> {
        &self.dependencies
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::dependencies::cargo_lock::Version;
    use crate::dependencies::rust_toolchain::Channel;
    use assert_fs::prelude::*;

    #[tokio::test]
    async fn finds_rust_toolchain() {
        let workspace_root = assert_fs::TempDir::new().unwrap();

        workspace_root
            .child("Cargo.toml")
            .write_str(
                r#"
[package]
name = "root"
version = "0.0.0"
[lib]
path = "lib.rs"
"#,
            )
            .unwrap();

        workspace_root
            .child("rust-toolchain.toml")
            .write_str(
                r#"
[toolchain]
channel = "nightly"
"#,
            )
            .unwrap();

        let dm = DependencyManager::new(workspace_root.path());

        dm.prepare().await.unwrap();

        let rust_toolchain = dm.rust_toolchain();
        dbg!(&rust_toolchain);

        assert_eq!(*rust_toolchain.toolchain().channel(), Channel::Nightly);
    }

    #[tokio::test]
    async fn finds_all_cargo_manifests() {
        let workspace_root = assert_fs::TempDir::new().unwrap();

        workspace_root
            .child("Cargo.toml")
            .write_str(
                r#"
[workspace]
members = [ "a", "b/c/d" ]
"#,
            )
            .unwrap();

        workspace_root
            .child("a/Cargo.toml")
            .write_str(
                r#"
[package]
name = "a"
version = "0.0.0"
[lib]
path = "lib.rs"
"#,
            )
            .unwrap();

        workspace_root
            .child("b/c/d/Cargo.toml")
            .write_str(
                r#"
[package]
name = "b_c_d"
version = "0.0.0"
[[bin]]
name = "bin"
path = "main.rs"
"#,
            )
            .unwrap();

        let dm = DependencyManager::new(workspace_root.path());

        dm.prepare().await.unwrap();

        let manifests = dm.cargo_manifests();
        dbg!(&manifests);
        assert_eq!(manifests.len(), 3);

        let manifest = manifests.get(&PathBuf::from("Cargo.toml")).unwrap();
        assert!(manifest.is_workspace());

        let manifest = manifests.get(&PathBuf::from("a/Cargo.toml")).unwrap();
        assert_eq!(manifest.package().unwrap().name(), "a".to_string());

        let manifest = manifests.get(&PathBuf::from("b/c/d/Cargo.toml")).unwrap();
        assert_eq!(manifest.package().unwrap().name(), "b_c_d".to_string());
    }

    #[tokio::test]
    async fn finds_cargo_locks() {
        let workspace_root = assert_fs::TempDir::new().unwrap();

        workspace_root
            .child("Cargo.toml")
            .write_str(
                r#"
[package]
name = "root"
version = "0.0.0"
[lib]
path = "lib.rs"
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
[[package]]
name = "async-stream"
version = "0.3.3"
source = "registry+https://github.com/rust-lang/crates.io-index"
checksum = "dad5c83079eae9969be7fadefe640a1c566901f05ff91ab221de4b6f68d9507e"
dependencies = [
 "async-stream-impl",
 "futures-core",
]
"#,
            )
            .unwrap();

        let dm = DependencyManager::new(workspace_root.path());

        dm.prepare().await.unwrap();

        let lockfiles = dm.cargo_locks();
        dbg!(&lockfiles);
        assert_eq!(lockfiles.len(), 1);

        let lockfile = lockfiles.get(&PathBuf::from("Cargo.lock")).unwrap();
        assert_eq!(*lockfile.version(), Version::V3);
        assert_eq!(lockfile.packages().len(), 2);
    }

    #[tokio::test]
    async fn consolidates_dependencies() {
        let workspace_root = assert_fs::TempDir::new().unwrap();

        workspace_root
            .child("Cargo.toml")
            .write_str(
                r#"
[package]
name = "root"
version = "0.0.0"
[lib]
path = "src/lib.rs"
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
[[package]]
name = "async-stream"
version = "0.3.3"
source = "registry+https://github.com/rust-lang/crates.io-index"
checksum = "dad5c83079eae9969be7fadefe640a1c566901f05ff91ab221de4b6f68d9507e"
dependencies = [
 "async-stream-impl",
 "futures-core",
]
"#,
            )
            .unwrap();

        let dm = DependencyManager::new(workspace_root.path());

        dm.prepare().await.unwrap();

        let deps = dm.dependencies();
        dbg!(&deps);
        assert_eq!(deps.len(), 2);

        let dep = deps.get("async-stream").unwrap()[0].clone();
        assert_eq!(dep.name(), "async-stream");
        assert_eq!(dep.version().to_string(), "0.3.3".to_string());
        assert_eq!(dep.dependencies().len(), 2);

        let dep = deps.get("anyhow").unwrap()[0].clone();
        assert_eq!(dep.name(), "anyhow");
        assert_eq!(dep.version().to_string(), "1.0.68".to_string());
        assert_eq!(dep.dependencies().len(), 0);
    }

    #[tokio::test]
    async fn respects_workspace_dependencies() {
        let workspace_root = assert_fs::TempDir::new().unwrap();

        workspace_root
            .child("Cargo.toml")
            .write_str(
                r#"
[workspace]
members = [ "a/b" ]
[workspace.dependencies]
anyhow = "1"
"#,
            )
            .unwrap();

        workspace_root
            .child("a/b/Cargo.toml")
            .write_str(
                r#"
[package]
name = "a-b"
version = "0.0.0"
[lib]
path = "lib.rs"
[dependencies]
anyhow = { workspace = true }
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

        let dm = DependencyManager::new(workspace_root.path());

        dm.prepare().await.unwrap();

        let manifests = dm.cargo_manifests();
        dbg!(&manifests);
        assert_eq!(manifests.len(), 2);

        let package = manifests.get(Path::new("a/b/Cargo.toml")).unwrap();
        assert_eq!(package.package().unwrap().name(), "a-b");
    }
}
