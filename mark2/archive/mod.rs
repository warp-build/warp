mod manager;
pub use manager::*;

use std::path::PathBuf;

/// An archive represents a file or directory that was downloaded.
///
#[derive(Builder, Debug)]
pub struct Archive {
    /// The final path on disk an archive has. Usually this is prefixed by `/warp/archives`
    final_path: PathBuf,

    /// The hash of the downloaded contents.
    hash: String,
}

impl Archive {
    pub fn builder() -> ArchiveBuilder {
        ArchiveBuilder::default()
    }

    pub fn final_path(&self) -> &PathBuf {
        &self.final_path
    }

    pub fn hash(&self) -> &str {
        self.hash.as_ref()
    }
}
