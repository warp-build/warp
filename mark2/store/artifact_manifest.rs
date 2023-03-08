use crate::util::from_file::FromFileError;
use crate::util::serde::*;
use serde_derive::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use thiserror::Error;
use tokio::io::AsyncReadExt;

use super::ArtifactId;

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

#[derive(Default, Builder, Debug, Clone, Serialize, Deserialize)]
#[builder(build_fn(error = "ArtifactManifestError"))]
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
    shell_env: BTreeMap<String, String>,
}

impl ArtifactManifest {
    pub fn builder() -> ArtifactManifestBuilder {
        Default::default()
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

    /// The identifier of this artifact.
    pub fn id(&self) -> ArtifactId {
        ArtifactId::new(&self.hash)
    }

    /// The original target string used to compute this artifact.
    pub fn target(&self) -> &str {
        self.target.as_ref()
    }

    /// The collection of files that this artifact provides. Typically this includes binary
    /// executables for the current architecture that can be used by dependants.
    pub fn provided_files(&self) -> &BTreeMap<String, PathBuf> {
        &self.provides
    }

    /// Finds by name a file amongst the files provided by this artifact.
    pub fn provides(&self, binary: &str) -> Option<&PathBuf> {
        self.provides.get(binary)
    }

    /// The name of the rule used to compute this artifact.
    pub fn rule_name(&self) -> &str {
        self.rule_name.as_ref()
    }

    /// The sources used to compute this artifact.
    pub fn srcs(&self) -> &[PathBuf] {
        self.srcs.as_ref()
    }

    /// The files that were created when computing this artifact.
    pub fn outs(&self) -> &[PathBuf] {
        self.outs.as_ref()
    }

    /// The shell environment that was used when computing this artifact.
    pub fn shell_env(&self) -> &BTreeMap<String, String> {
        &self.shell_env
    }

    /// The hash of the configuration, inputs and outputs of this artifact.
    pub fn hash(&self) -> &str {
        self.hash.as_ref()
    }
}

#[derive(Error, Debug)]
pub enum ArtifactManifestError {
    #[error(transparent)]
    BuilderError(derive_builder::UninitializedFieldError),
}

impl From<derive_builder::UninitializedFieldError> for ArtifactManifestError {
    fn from(value: derive_builder::UninitializedFieldError) -> Self {
        ArtifactManifestError::BuilderError(value)
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
