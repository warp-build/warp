use crate::util::from_file::{FromFile, FromFileError};
use chrono::{DateTime, Utc};
use serde_derive::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::path::Path;
use thiserror::Error;
use tokio::io::AsyncReadExt;

pub const PUBLISH_MANIFEST_FILE: &str = "Manifest.json";

/// A manifest used for describing a published package with Warp.
///
#[derive(Default, Debug, Clone, Serialize, Deserialize)]
pub struct PackageManifest {
    #[serde(with = "crate::util::serde::iso8601")]
    published_at: DateTime<Utc>,

    keys: BTreeMap<String, Vec<String>>,
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

    /// The time at which this manifest was published
    pub fn published_at(&self) -> DateTime<Utc> {
        self.published_at
    }

    /// The keys to retrieve from the cache when downloading this manifest. They are grouped by the
    /// hash of the environment used to build them.
    ///
    /// So for example, if you see:
    ///
    /// ```json
    /// {
    ///     "keys": {
    ///         "aarm64-darwin-macos": [...]
    ///         "x86_64-unknown-linux": [...]
    ///     }
    /// }
    /// ```
    pub fn keys(&self) -> &BTreeMap<String, Vec<String>> {
        &self.keys
    }
}

#[derive(Error, Debug)]
pub enum PackageManifestError {}

#[cfg(test)]
mod tests {
    use super::*;
    use assert_fs::prelude::*;

    #[tokio::test]
    async fn read_from_file() {
        let dir = assert_fs::TempDir::new().unwrap();

        let manifest = dir.child("Manifest.json");
        manifest
            .write_str(
                r#"

{
    "published_at": "2023-03-01T21:09:32+00:00",
    "keys": {
        "aarm64-apple-macos": [ "a-hash" ]
    }
}

        "#,
            )
            .unwrap();

        let package_manifest = PackageManifest::from_file(manifest.path()).await.unwrap();

        let mut expected = BTreeMap::default();
        expected.insert("aarm64-apple-macos".to_string(), vec!["a-hash".to_string()]);
        assert_eq!(*package_manifest.keys(), expected);

        assert_eq!(
            package_manifest.published_at().to_rfc3339(),
            "2023-03-01T21:09:32+00:00"
        );
    }
}
