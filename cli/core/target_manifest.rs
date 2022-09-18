use super::*;
use serde_derive::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use thiserror::*;
use tokio::fs;
use tokio::io::AsyncReadExt;

#[derive(Error, Debug)]
pub enum TargetManifestError {
    #[error("Could not parse Manifest file: {0:?}")]
    ParseError(toml::de::Error),

    #[error("Could not print Manifest file: {0:#?}")]
    PrintError(toml::ser::Error),

    #[error(transparent)]
    IOError(std::io::Error),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TargetManifest {
    pub label: String,
    pub rule_name: String,
    pub hash: String,
    pub cached: bool,
    pub is_valid: bool,

    pub srcs: Vec<PathBuf>,
    pub outs: Vec<PathBuf>,
    pub provides: BTreeMap<String, PathBuf>,

    pub deps: BTreeMap<String, String>,
    pub transitive_deps: BTreeMap<String, String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TargetManifestFile {
    pub manifest: TargetManifest,
}

impl TargetManifest {
    #[tracing::instrument(name = "TargetManifest::from_validation_result")]
    pub fn from_validation_result(
        validation: &ValidationStatus,
        store_path: &Path,
        target: &ExecutableTarget,
    ) -> Self {
        Self {
            cached: false,

            is_valid: matches!(&validation, ValidationStatus::Valid { .. }),

            hash: target.hash.clone(),

            label: target.label.to_string(),

            rule_name: target.rule.name.clone(),

            deps: target
                .deps
                .iter()
                .cloned()
                .map(|d| (d.label.to_string(), d.hash))
                .collect(),

            transitive_deps: target
                .transitive_deps
                .iter()
                .cloned()
                .map(|d| (d.label.to_string(), d.hash))
                .collect(),

            provides: target
                .provides
                .iter()
                .map(|(name, p)| (name.clone(), store_path.join(p)))
                .collect(),

            outs: if let ValidationStatus::Valid { outputs } = validation {
                outputs.clone()
            } else {
                target.outs.iter().cloned().collect()
            },

            srcs: target.srcs.iter().cloned().collect(),
        }
    }

    #[tracing::instrument(name = "TargetManifest::from_file")]
    pub async fn from_file(path: &Path) -> Result<Self, TargetManifestError> {
        let mut bytes = vec![];

        let mut file = fs::File::open(path)
            .await
            .map_err(TargetManifestError::IOError)?;

        file.read_to_end(&mut bytes)
            .await
            .map_err(TargetManifestError::IOError)?;

        let target_manifest_file: TargetManifestFile =
            toml::from_slice(&bytes).map_err(TargetManifestError::ParseError)?;

        Ok(target_manifest_file.manifest)
    }

    #[tracing::instrument(name = "TargetManifest::write")]
    pub async fn write(&self, root: &Path) -> Result<(), TargetManifestError> {
        let mut manifest = self.clone();
        manifest.cached = true;

        let toml = toml::to_string_pretty(&TargetManifestFile { manifest })
            .map_err(TargetManifestError::PrintError)?;

        fs::write(&root.join("Manifest.toml"), toml)
            .await
            .map_err(TargetManifestError::IOError)
    }
}
