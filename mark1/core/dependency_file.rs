use super::*;
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::io::BufReader;
use std::path::{Path, PathBuf};
use thiserror::*;
use tokio::fs;
use tracing::*;

pub const DEPENDENCIES_JSON: &str = "Dependencies.json";

#[derive(Error, Debug)]
pub enum DependencyFileError {
    #[error("Could not parse Dependency file: {0:?}")]
    ParseError(serde_json::Error),

    #[error("Could not print Dependency file: {0:#?}")]
    PrintError(serde_json::Error),

    #[error("Could not open dependency file at {path:?} due to: {err:?}")]
    CouldNotOpen { path: PathBuf, err: std::io::Error },

    #[error("Could not write dependency file at {path:?} due to: {err:?}")]
    CouldNotWrite { path: PathBuf, err: std::io::Error },

    #[error("Could not find workspace a file walking upwards your file system. Are you sure we're in the right place?")]
    DependencyFileNotFound,

    #[error("Attempted to build a Dependency while missing fields: {0:?}")]
    BuilderError(derive_builder::UninitializedFieldError),

    #[error("{0}")]
    ValidationError(String),
}

impl From<derive_builder::UninitializedFieldError> for DependencyFileError {
    fn from(err: derive_builder::UninitializedFieldError) -> Self {
        Self::BuilderError(err)
    }
}

impl From<String> for DependencyFileError {
    fn from(s: String) -> Self {
        Self::ValidationError(s)
    }
}

/// A struct representing a `Dependency.json` file in a Warp workspace.
///
/// This is primarily used for serialization/deserialization, and manipualting the file itself
/// through a semantic API that hides the json disk representation.
///
#[derive(Clone, Default, Debug, Builder, Serialize, Deserialize)]
#[builder(build_fn(error = "DependencyFileError"))]
pub struct DependencyFile {
    #[builder(default)]
    #[serde(default)]
    pub version: String,

    #[builder(default)]
    #[serde(default)]
    pub dependencies: BTreeMap<String, DependencyJson>,
}

impl DependencyFile {
    pub fn builder() -> DependencyFileBuilder {
        DependencyFileBuilder::default()
    }

    #[tracing::instrument(name = "DependencyFile::read_from_file")]
    pub async fn read_from_file(path: &Path) -> Result<Self, DependencyFileError> {
        if fs::metadata(&path).await.is_err() {
            let dep_file = Self::builder().version("0".to_string()).build().unwrap();
            dep_file.write(path).await?;
        }

        let file = fs::File::open(path)
            .await
            .map_err(|err| DependencyFileError::CouldNotOpen {
                path: path.to_path_buf(),
                err,
            })?;

        let reader = json_comments::StripComments::new(BufReader::new(file.into_std().await));

        serde_json::from_reader(reader).map_err(DependencyFileError::ParseError)
    }

    #[tracing::instrument(name = "DependencyFile::write")]
    pub async fn write(&self, root: &Path) -> Result<(), DependencyFileError> {
        let json = serde_json::to_string_pretty(&self).map_err(DependencyFileError::PrintError)?;
        fs::write(&root, json)
            .await
            .map_err(|err| DependencyFileError::CouldNotWrite {
                path: root.to_path_buf(),
                err,
            })
    }
}

#[derive(Debug, Clone, Builder, Serialize, Deserialize)]
pub struct DependencyJson {
    #[builder(default)]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub resolver: Option<String>,

    pub version: String,

    pub package: String,

    pub url: url::Url,
}

impl DependencyJson {
    pub fn builder() -> DependencyJsonBuilder {
        DependencyJsonBuilder::default()
    }
}
