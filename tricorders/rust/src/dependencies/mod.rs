mod rust_toolchain;

use self::rust_toolchain::{RustToolchain, RustToolchainError};
use cargo_toml::Manifest;
use dashmap::DashMap;
use std::path::{Path, PathBuf};
use std::sync::{Arc, RwLock};
use thiserror::Error;
use tokio::fs;

#[derive(Default, Clone, Eq, PartialEq)]
pub enum Status {
    #[default]
    Idle,
    Loading,
    Ready,
}

#[derive(Default)]
pub struct DependencyManager {
    status: Arc<RwLock<Status>>,
    workspace_root: PathBuf,
    cargo_manifests: DashMap<PathBuf, Manifest>,
    rust_toolchain: Arc<RwLock<RustToolchain>>,
}

impl DependencyManager {
    pub fn new(workspace_root: &Path) -> Self {
        Self {
            workspace_root: workspace_root.to_path_buf(),
            ..Default::default()
        }
    }

    pub async fn prepare(&self) -> Result<(), DependencyManagerError> {
        self.mark_as_loading();
        self.load_manifests().await?;
        self.mark_as_ready();
        Ok(())
    }

    async fn load_manifests(&self) -> Result<(), DependencyManagerError> {
        let root = self.workspace_root.clone();

        let gitignore = self.workspace_root.join(".gitignore");
        let gitignore = gitignore::File::new(&gitignore).ok();

        let mut dirs = vec![root.clone()];

        while let Some(dir) = dirs.pop() {
            let mut read_dir = fs::read_dir(&dir).await.unwrap();

            while let Ok(Some(entry)) = read_dir.next_entry().await {
                let path = entry.path().clone();

                if let Some(gitignore) = &gitignore {
                    if gitignore.is_excluded(&path).unwrap_or_default() {
                        continue;
                    }
                }

                if fs::read_dir(&path).await.is_ok() {
                    dirs.push(path.clone());
                    continue;
                };

                match path.file_name().unwrap().to_string_lossy().as_ref() {
                    "Cargo.toml" => {
                        let cargo_manifest = Manifest::from_path(&path)?;
                        let path = path.strip_prefix(&root).unwrap().to_path_buf();
                        self.cargo_manifests.insert(path, cargo_manifest);
                    }
                    "rust-toolchain.toml" => {
                        let rust_toolchain = RustToolchain::from_path(&path).await?;
                        (*self.rust_toolchain.try_write().unwrap()) = rust_toolchain;
                    }
                    _ => {}
                }
            }
        }

        Ok(())
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

    pub fn cargo_manifests(&self) -> DashMap<PathBuf, Manifest> {
        self.cargo_manifests.clone()
    }

    pub fn rust_toolchain(&self) -> RustToolchain {
        self.rust_toolchain.try_read().unwrap().clone()
    }
}

#[derive(Error, Debug)]
pub enum DependencyManagerError {
    #[error(transparent)]
    CargoTomlError(cargo_toml::Error),

    #[error(transparent)]
    RustToolchainError(RustToolchainError),
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::dependencies::rust_toolchain::Channel;
    use assert_fs::prelude::*;

    #[tokio::test]
    async fn finds_rust_toolchain() {
        let workspace_root = assert_fs::TempDir::new().unwrap();

        workspace_root
            .child("Cargo.toml")
            .write_str(
                r#"
name = "root"
version = "0.0.0"
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
name = "root"
version = "0.0.0"
"#,
            )
            .unwrap();

        workspace_root
            .child("a/Cargo.toml")
            .write_str(
                r#"
name = "a"
version = "0.0.0"
"#,
            )
            .unwrap();

        workspace_root
            .child("b/c/d/Cargo.toml")
            .write_str(
                r#"
name = "b_c_d"
version = "0.0.0"
"#,
            )
            .unwrap();

        let dm = DependencyManager::new(workspace_root.path());

        dm.prepare().await.unwrap();

        let manifests = dm.cargo_manifests();
        dbg!(&manifests);
        assert_eq!(manifests.len(), 3);

        let manifest = manifests.get(&PathBuf::from("Cargo.toml")).unwrap();
        assert_eq!(manifest.package().name, "root".to_string());

        let manifest = manifests.get(&PathBuf::from("a/Cargo.toml")).unwrap();
        assert_eq!(manifest.package().name, "a".to_string());

        let manifest = manifests.get(&PathBuf::from("b/c/d/Cargo.toml")).unwrap();
        assert_eq!(manifest.package().name, "b_c_d".to_string());
    }
}
