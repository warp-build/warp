mod manager;
pub use manager::*;
use url::Url;

use std::path::PathBuf;

/// An archive represents a file or directory that was downloaded.
///
#[derive(Builder, Debug)]
pub struct Archive {
    #[builder(setter(into))]
    final_path: PathBuf,

    #[builder(setter(into))]
    hash: String,

    url: Url,
}

impl Archive {
    pub fn builder() -> ArchiveBuilder {
        ArchiveBuilder::default()
    }

    /// The final path on disk an archive has. Usually this is prefixed by `/warp/archives`
    pub fn final_path(&self) -> &PathBuf {
        &self.final_path
    }

    /// The hash of the downloaded contents.
    pub fn hash(&self) -> &str {
        self.hash.as_ref()
    }

    pub fn url(&self) -> &Url {
        &self.url
    }

    pub fn set_final_path(&mut self, final_path: PathBuf) {
        self.final_path = final_path;
    }
}
