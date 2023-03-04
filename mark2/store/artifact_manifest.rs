use crate::util::from_file::FromFileError;
use crate::util::serde::*;
use serde_derive::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use tokio::io::AsyncReadExt;

pub const ARTIFACT_MANIFEST_FILE: &str = "Manifest.json";

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BuildStamps {
    #[serde(with = "iso8601")]
    pub plan_started_at: chrono::DateTime<chrono::Utc>,
    #[serde(with = "iso8601")]
    pub plan_ended_at: chrono::DateTime<chrono::Utc>,
    #[serde(with = "duration")]
    pub plan_elapsed_time: chrono::Duration,
    #[serde(with = "iso8601")]
    pub build_started_at: chrono::DateTime<chrono::Utc>,
    #[serde(with = "iso8601")]
    pub build_completed_at: chrono::DateTime<chrono::Utc>,
    #[serde(with = "duration")]
    pub build_elapsed_time: chrono::Duration,
}

impl Default for BuildStamps {
    fn default() -> Self {
        Self {
            plan_started_at: Default::default(),
            plan_ended_at: Default::default(),
            plan_elapsed_time: chrono::Duration::zero(),
            build_started_at: Default::default(),
            build_completed_at: Default::default(),
            build_elapsed_time: chrono::Duration::zero(),
        }
    }
}

#[derive(Default, Debug, Clone, Serialize, Deserialize)]
pub enum ArtifactManifestVersion {
    #[default]
    V0,
}

#[derive(Default, Debug, Clone, Serialize, Deserialize)]
pub struct ArtifactManifest {
    version: ArtifactManifestVersion,

    target: String,

    rule_name: String,

    hash: String,

    #[serde(default)]
    store_path: PathBuf,

    #[serde(default)]
    cached: bool,

    #[serde(default)]
    is_valid: bool,

    #[serde(default)]
    srcs: Vec<PathBuf>,

    #[serde(default)]
    outs: Vec<PathBuf>,

    #[serde(default)]
    buildstamps: BuildStamps,

    #[serde(default)]
    provides: BTreeMap<String, PathBuf>,

    #[serde(default)]
    deps: BTreeMap<String, String>,

    #[serde(default)]
    runtime_deps: BTreeMap<String, String>,

    #[serde(default)]
    transitive_deps: BTreeMap<String, String>,

    #[serde(default)]
    toolchains: BTreeMap<String, String>,

    #[serde(default)]
    env: BTreeMap<String, String>,
}

impl ArtifactManifest {
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

    pub fn target(&self) -> &str {
        self.target.as_ref()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use assert_fs::prelude::*;

    #[tokio::test]
    async fn read_from_file() {
        let dir = assert_fs::TempDir::new().unwrap();

        let manifest = dir.child("Manifest.json");
        let expected = include_str!("./fixtures/Manifest.json");
        manifest.write_str(expected).unwrap();

        let artifact_manifest = ArtifactManifest::from_file(manifest.path()).await.unwrap();

        let actual = serde_json::to_string_pretty(&artifact_manifest).unwrap();

        assert_eq!(format!("{actual}\n"), expected);
    }
}
