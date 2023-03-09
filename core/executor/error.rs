use crate::model::ConcreteTarget;
use crate::store::{ArtifactManifestError, StoreError};
use std::path::PathBuf;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ExecutorError {
    #[error(transparent)]
    StoreError(StoreError),

    #[error(transparent)]
    ArtifactManifestError(ArtifactManifestError),

    #[error("When building {}, could not create directory {dst:?} at: {dst_parent:?}", .target.to_string())]
    CouldNotCreateDir {
        target: ConcreteTarget,
        dst: PathBuf,
        dst_parent: PathBuf,
    },

    #[error(
        r#"

When building {}, could not copy

  {}

into sandbox at

  {}

due to:

  {err:?}

  "#, .target.to_string(),
  .src.to_string_lossy(),
  .dst.to_string_lossy(),
    )]
    CouldNotCopy {
        target: ConcreteTarget,
        src: PathBuf,
        dst: PathBuf,
        err: std::io::Error,
    },

    #[error(transparent)]
    Unknown(anyhow::Error),
}

impl From<StoreError> for ExecutorError {
    fn from(value: StoreError) -> Self {
        ExecutorError::StoreError(value)
    }
}

impl From<ArtifactManifestError> for ExecutorError {
    fn from(value: ArtifactManifestError) -> Self {
        ExecutorError::ArtifactManifestError(value)
    }
}

impl From<anyhow::Error> for ExecutorError {
    fn from(value: anyhow::Error) -> Self {
        ExecutorError::Unknown(value)
    }
}