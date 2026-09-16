use thiserror::Error;

use super::PackageManifest;
use crate::archive::Archive;
use crate::model::FsTarget;
use crate::util::from_file::FromFileError;
use crate::Target;
use std::path::{Path, PathBuf};

pub const MANIFEST_FILE: &str = "manifest.json";

#[derive(Builder, Debug)]
pub struct Package {
    main: Archive,
    manifest: PackageManifest,
    target: Target,
    deps: Vec<Archive>,
}

impl Package {
    pub fn builder() -> PackageBuilder {
        Default::default()
    }

    pub fn manifest(&self) -> &PackageManifest {
        &self.manifest
    }

    pub fn main(&self) -> &Archive {
        &self.main
    }

    pub fn deps(&self) -> &[Archive] {
        self.deps.as_ref()
    }

    pub async fn update_manifest_file(
        &self,
        manifest_root: &Path,
    ) -> Result<(), UpdateManifestError> {
        let Some(target) = self.target.as_local() else {
            return Ok(());
        };

        let path = Self::get_local_manifest_path(manifest_root, target);

        let local_manifest = PackageManifest::from_file(&path).await.unwrap_or_default();
        let merged = local_manifest.merge(self.manifest().clone());
        merged.write(&path).await?;
        Ok(())
    }

    fn get_local_manifest_path(manifest_root: &Path, target: &FsTarget) -> PathBuf {
        manifest_root
            .join(target.path().parent().unwrap())
            .join(MANIFEST_FILE)
    }
}

#[derive(Error, Debug)]
pub enum UpdateManifestError {
    #[error(transparent)]
    CouldNotWriteManifest(FromFileError),
}

impl From<FromFileError> for UpdateManifestError {
    fn from(value: FromFileError) -> Self {
        UpdateManifestError::CouldNotWriteManifest(value)
    }
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use super::*;

    #[test]
    fn builds_local_manifest_path_with_dirs() {
        let target: FsTarget = Path::new("./tricorders/beam/mix.exs").into();

        let path = Package::get_local_manifest_path(Path::new("./store"), &target);

        assert_eq!(path, PathBuf::from("./store/tricorders/beam/manifest.json"));
    }

    #[test]
    fn builds_local_manifest_path_without_dirs() {
        let target: FsTarget = Path::new("./mix.exs").into();

        let path = Package::get_local_manifest_path(Path::new("./store"), &target);

        assert_eq!(path, PathBuf::from("./store/manifest.json"));
    }
}
