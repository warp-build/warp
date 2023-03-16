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
}
