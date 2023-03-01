use crate::util::from_file::{FromFile, FromFileError};
use async_trait::async_trait;
use chrono::{DateTime, Utc};
use serde_derive::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::path::Path;
use thiserror::Error;
use tokio::io::AsyncReadExt;

pub const PUBLISH_MANIFEST_FILE: &str = "manifest.json";

/// A manifest used for describing a published package with Warp.
///
#[derive(Default, Debug, Clone, Serialize, Deserialize)]
pub struct PackageManifest {
    /// The time at which this manifest was published
    #[serde(with = "crate::util::serde::iso8601")]
    published_at: DateTime<Utc>,

    /// The keys to retrieve from the cache when downloading this manifest. They are grouped by the
    /// hash of the environment used to build them.
    ///
    /// So for example, if you see:
    ///
    /// ```json
    /// {
    ///     "cache_keys": {
    ///         "aarm64-darwin-macos": [...]
    ///         "x86_64-unknown-linux": [...]
    ///     }
    /// }
    /// ```
    cache_keys: BTreeMap<String, Vec<String>>,
}

impl PackageManifest {
    // TODO(@ostera): fix util::FromFile to derive this function there
    pub async fn from_file(path: &Path) -> Result<Self, FromFileError> {
        let mut file =
            tokio::fs::File::open(&path)
                .await
                .map_err(|err| FromFileError::CouldNotOpenFile {
                    err,
                    file: path.to_path_buf(),
                })?;

        let mut bytes = vec![];
        file.read_to_end(&mut bytes)
            .await
            .map_err(|err| FromFileError::CouldNotReadFile {
                err,
                file: path.to_path_buf(),
            })?;

        serde_json::from_slice(&bytes).map_err(|err| FromFileError::ParseError {
            err,
            file: path.to_path_buf(),
            bytes: String::from_utf8_lossy(&bytes).to_string(),
        })
    }

    pub fn published_at(&self) -> DateTime<Utc> {
        self.published_at
    }

    pub fn cache_keys(&self) -> &BTreeMap<String, Vec<String>> {
        &self.cache_keys
    }
}

#[derive(Error, Debug)]
pub enum PackageManifestError {}
