mod artifact_id;
mod default;
mod error;
mod manifest_url;
mod package_manifest;
mod public_store;

pub use artifact_id::*;
pub use default::*;
pub use error::*;
pub use manifest_url::*;
pub use package_manifest::*;
use public_store::*;

use async_trait::async_trait;

const DEFAULT_WARP_STORE_HOST: &str = "store.warp.build";

#[async_trait]
pub trait Store {
    async fn install_from_manifest_url(&self, url: &ManifestUrl) -> Result<(), StoreError>;
}
