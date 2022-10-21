use super::*;
use serde_derive::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::io::BufReader;
use std::path::Path;
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

    #[error(transparent)]
    IOError(std::io::Error),

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
    pub dependencies: BTreeMap<String, String>,
}

impl DependencyFile {
    pub fn builder() -> DependencyFileBuilder {
        DependencyFileBuilder::default()
    }

    #[tracing::instrument(name = "DependencyFile::read_from_file")]
    pub async fn read_from_file(path: &Path) -> Result<Self, DependencyFileError> {
        let file = fs::File::open(path)
            .await
            .map_err(DependencyFileError::IOError)?;

        let reader = json_comments::StripComments::new(BufReader::new(file.into_std().await));

        serde_json::from_reader(reader).map_err(DependencyFileError::ParseError)
    }

    #[tracing::instrument(name = "DependencyFile::write")]
    pub async fn write(&self, root: &Path) -> Result<(), DependencyFileError> {
        let json = serde_json::to_string_pretty(&self).map_err(DependencyFileError::PrintError)?;
        fs::write(&root.join(DEPENDENCIES_JSON), json)
            .await
            .map_err(DependencyFileError::IOError)
    }
}
