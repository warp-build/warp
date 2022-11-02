use super::*;
use serde_derive::{Deserialize, Serialize};
use std::path::{Path, PathBuf};
use thiserror::*;
use tokio::fs;
use tokio::io::AsyncReadExt;
use tracing::*;

pub const BUILDSTAMP: &str = "output.json";

#[derive(Error, Debug)]
pub enum OutputManifestError {
    #[error(
        "Could not parse Output Manifest file at {file:?} due to: {err:?}, full manifest:\n {json}"
    )]
    ParseError {
        file: PathBuf,
        err: serde_json::Error,
        json: String,
    },

    #[error("Could not print Output Manifest file: {0:#?}")]
    PrintError(serde_json::Error),

    #[error(transparent)]
    IOError(std::io::Error),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OutputManifestHash {
    pub label: Label,
    pub hash: String,
}

impl OutputManifestHash {
    #[tracing::instrument(name = "OutputManifestHash::find", skip(path))]
    pub async fn find(label: &Label, path: &Path) -> Result<Self, OutputManifestError> {
        let path = OutputManifest::_file(label, path);
        let mut file = fs::File::open(&path)
            .await
            .map_err(OutputManifestError::IOError)?;

        let mut bytes = vec![];
        file.read_to_end(&mut bytes)
            .await
            .map_err(OutputManifestError::IOError)?;

        serde_json::from_slice(&bytes).map_err(|err| OutputManifestError::ParseError {
            file: path,
            err,
            json: String::from_utf8(bytes).unwrap(),
        })
    }

    #[tracing::instrument(name = "OutputManifestHash::write", skip(self))]
    pub async fn write(&self, root: &PathBuf) -> Result<(), OutputManifestError> {
        let json = serde_json::to_string_pretty(&self).map_err(OutputManifestError::PrintError)?;

        let file = OutputManifest::_file(&self.label, root);

        fs::write(file, json)
            .await
            .map_err(OutputManifestError::IOError)
    }

    fn _file(label: &Label, root: &Path) -> PathBuf {
        root.join(&label.hash().to_string())
            .with_extension(BUILDSTAMP)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OutputManifest {
    pub label: Label,
    pub hash: String,
    pub outs: Vec<PathBuf>,
}

impl OutputManifest {
    #[tracing::instrument(name = "OutputManifest::find", skip(path))]
    pub async fn find(label: &Label, path: &Path) -> Result<Self, OutputManifestError> {
        let path = OutputManifest::_file(label, path);
        let mut file = fs::File::open(&path)
            .await
            .map_err(OutputManifestError::IOError)?;

        let mut bytes = vec![];
        file.read_to_end(&mut bytes)
            .await
            .map_err(OutputManifestError::IOError)?;

        serde_json::from_slice(&bytes).map_err(|err| OutputManifestError::ParseError {
            file: path,
            err,
            json: String::from_utf8(bytes).unwrap(),
        })
    }

    #[tracing::instrument(name = "OutputManifest::write", skip(self))]
    pub async fn write(&self, root: &PathBuf) -> Result<(), OutputManifestError> {
        let json = serde_json::to_string_pretty(&self).map_err(OutputManifestError::PrintError)?;

        let file = OutputManifest::_file(&self.label, root);

        fs::write(file, json)
            .await
            .map_err(OutputManifestError::IOError)
    }

    fn _file(label: &Label, root: &Path) -> PathBuf {
        root.join(&label.hash().to_string())
            .with_extension(BUILDSTAMP)
    }
}
