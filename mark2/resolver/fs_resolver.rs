use std::path::PathBuf;

use thiserror::*;

use super::{FsTarget, Goal, ResolverError};

/// The `FsResolver` knows how to resolve a particular `Target` by looking into the file system and
/// determining if this is in fact a file on disk, and sending the sources to a `Tricorder` for
/// analysis.
#[derive(Clone, Debug)]
pub struct FsResolver;

impl FsResolver {
    pub fn new() -> Self {
        Self
    }

    pub async fn resolve(
        &self,
        _goal: Goal,
        target: &FsTarget,
    ) -> Result<PathBuf, FsResolverError> {
        if let Ok(_) = tokio::fs::metadata(target.path()).await {
            return Ok(target.path().clone());
        };
        Err(FsResolverError::CouldNotFindFile {
            path: target.path().clone(),
        })
    }
}

#[derive(Error, Debug)]
pub enum FsResolverError {
    #[error("Something went wrong while resolving a file")]
    Unknown,
    #[error("Could not open file at {path:?}")]
    CouldNotFindFile { path: PathBuf },
}

impl From<FsResolverError> for ResolverError {
    fn from(value: FsResolverError) -> Self {
        Self::FsResolverError(value)
    }
}
