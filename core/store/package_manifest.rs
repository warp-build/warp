use crate::sync::Arc;
use crate::util::from_file::FromFileError;
use chrono::{DateTime, Utc};
use fxhash::FxHashSet;
use serde_derive::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::path::Path;
use thiserror::Error;
use tokio::fs;
use tokio::io::AsyncReadExt;

use super::ArtifactManifest;

pub const PUBLISH_MANIFEST_FILE: &str = "Manifest.json";

/// A manifest used for describing a published package with Warp.
///
#[derive(Builder, Default, Debug, Clone, Serialize, Deserialize)]
pub struct PackageManifest {
    #[serde(with = "crate::util::serde::iso8601")]
    #[builder(default = "Utc::now()")]
    published_at: DateTime<Utc>,
    keys: BTreeMap<String, Vec<String>>,
}

impl PackageManifest {
    pub fn builder() -> PackageManifestBuilder {
        Default::default()
    }

    pub fn from_artifact_manifest(manifest: Arc<ArtifactManifest>) -> Self {
        let mut manifest_keys = BTreeMap::new();

        let host_triple = manifest.exec_env().get("host_triple").unwrap();

        let mut hashes: Vec<String> = vec![manifest.hash().to_string()];

        let mut uniq_hashes: FxHashSet<String> = Default::default();

        for (_, dep_hash) in manifest
            .deps()
            .iter()
            .chain(manifest.runtime_deps().iter())
            .chain(manifest.transitive_deps().iter())
            .chain(manifest.toolchains().iter())
        {
            uniq_hashes.insert(dep_hash.to_string());
        }

        hashes.extend(uniq_hashes.into_iter());

        manifest_keys.insert(host_triple.clone(), hashes.clone());

        Self::builder().keys(manifest_keys).build().unwrap()
    }

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

    pub async fn write(&self, path: &Path) -> Result<(), FromFileError> {
        let json = serde_json::to_string_pretty(&self).unwrap();
        fs::write(&path, json)
            .await
            .map_err(|err| FromFileError::CouldNotWriteFile {
                err,
                file: path.to_path_buf(),
            })
    }

    // replaces the existing keys with those of a new manifest
    pub fn merge(&self, other: &Self) -> Self {
        let mut keys = self.keys.clone();

        for (key, value) in other.keys.iter() {
            keys.insert(key.clone(), value.clone());
        }

        Self::builder().keys(keys).build().unwrap()
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
