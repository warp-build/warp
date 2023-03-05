mod artifact_id;
mod artifact_manifest;
mod default;
mod error;
mod local;
mod manifest_url;
mod package_manifest;
mod public;

use std::path::PathBuf;

pub use artifact_id::*;
pub use artifact_manifest::*;
pub use default::*;
pub use error::*;
use local::*;
pub use manifest_url::*;
pub use package_manifest::*;
use public::*;

use async_trait::async_trait;

const DEFAULT_WARP_STORE_HOST: &str = "store.warp.build";

#[async_trait]
pub trait Store {
    async fn install_from_manifest_url(
        &self,
        url: &ManifestUrl,
    ) -> Result<ArtifactManifest, StoreError>;

    fn canonicalize_provided_artifact<N: AsRef<str>>(
        &self,
        am: &ArtifactManifest,
        name: N,
    ) -> Option<PathBuf>;
}
