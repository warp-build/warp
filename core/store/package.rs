use crate::archive::Archive;

use super::PackageManifest;

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
}
