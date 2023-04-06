use super::PackageManifest;
use crate::archive::Archive;
use crate::util::from_file::FromFileError;
use crate::Target;
use std::path::PathBuf;

pub const MANIFEST_FILE: &str = "Publish.json";

#[derive(Builder, Debug)]
pub struct Package {
    main: Archive,
    manifest: PackageManifest,
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

    pub async fn update(&self, target: Target) -> Result<(), FromFileError> {
        let path = self.get_local_manifest_path(&target);
        let local_manifest = PackageManifest::from_file(&path).await.unwrap_or_default();

        let new_manifest = self.manifest();
        let merged = local_manifest.merge(new_manifest);

        merged.write(&path).await
    }

    fn get_local_manifest_path(&self, target: &Target) -> PathBuf {
        let binding = PathBuf::default();
        let manifest = match target {
            Target::Fs(fs_target) => fs_target
                .path()
                .parent()
                .expect("Target should always have a parent folder"),
            _ => &binding,
        };

        manifest.join(MANIFEST_FILE)
    }
}
