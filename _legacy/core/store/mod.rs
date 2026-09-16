mod artifact_id;
mod artifact_manifest;
mod default;
mod error;
mod local;
mod package;
mod package_manifest;
mod packer;
mod public;

pub use artifact_id::*;
pub use artifact_manifest::*;
pub use default::*;
pub use error::*;
use local::*;
pub use package::*;
pub use package_manifest::*;
pub use packer::*;
use public::*;

use crate::model::ExecutableSpec;
use async_trait::async_trait;
use std::path::PathBuf;
use url::Url;

pub const MANIFEST_FILE: &str = "Manifest.json";

/// The Store trait represents an abstract artifact store that works with [ExecutableSpec]s and
/// [ArtifactManifests] to assist the build process.
///
#[async_trait]
pub trait Store: Sync + Send + Clone + Sized {
    async fn install_from_manifest_url(&self, url: &Url) -> Result<ArtifactManifest, StoreError>;

    async fn find(&self, spec: &ExecutableSpec) -> Result<Option<ArtifactManifest>, StoreError>;

    async fn clean(&self, spec: &ExecutableSpec) -> Result<(), StoreError>;

    async fn promote(&self, am: &ArtifactManifest) -> Result<(), StoreError>;

    async fn save(&self, spec: &ExecutableSpec, am: &ArtifactManifest) -> Result<(), StoreError>;

    fn get_local_store_path_for_spec(&self, spec: &ExecutableSpec) -> PathBuf;

    fn get_local_store_path_for_manifest(&self, spec: &ArtifactManifest) -> PathBuf;

    fn canonicalize_provided_artifact<N: AsRef<str>>(
        &self,
        am: &ArtifactManifest,
        name: N,
    ) -> Option<PathBuf>;
}
