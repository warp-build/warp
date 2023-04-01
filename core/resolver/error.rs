use crate::archive::ArchiveManagerError;
use crate::code::{CodeDatabaseError, CodeManagerError, SourceHasherError};
use crate::model::{FsTarget, TargetError};
use crate::tricorder::{TricorderError, TricorderManagerError};
use crate::{Goal, Target};
use std::path::PathBuf;
use thiserror::*;

#[derive(Error, Debug)]
pub enum ResolverError {
    #[error("Something went wrong: {0}")]
    Unknown(String),

    #[error("Could not resolve target {target:?} for goal {goal:?}")]
    CouldNotResolveTarget { goal: Goal, target: Target },

    #[error(transparent)]
    TricorderManagerError(TricorderManagerError),

    #[error(transparent)]
    TricorderError(TricorderError),

    #[error(transparent)]
    TargetError(TargetError),

    #[error(transparent)]
    ArchiveManagerError(ArchiveManagerError),

    #[error(transparent)]
    CodeDatabaseError(CodeDatabaseError),

    #[error("Could not open file at {path:?}, from target: {target:#?}")]
    CouldNotFindFile { path: PathBuf, target: FsTarget },

    #[error(transparent)]
    SourceHasherError(SourceHasherError),

    #[error(transparent)]
    CodeManagerError(CodeManagerError),
}

impl From<TricorderError> for ResolverError {
    fn from(value: TricorderError) -> Self {
        Self::TricorderError(value)
    }
}

impl From<TricorderManagerError> for ResolverError {
    fn from(value: TricorderManagerError) -> Self {
        Self::TricorderManagerError(value)
    }
}

impl From<TargetError> for ResolverError {
    fn from(value: TargetError) -> Self {
        ResolverError::TargetError(value)
    }
}

impl From<ArchiveManagerError> for ResolverError {
    fn from(value: ArchiveManagerError) -> Self {
        ResolverError::ArchiveManagerError(value)
    }
}

impl From<CodeDatabaseError> for ResolverError {
    fn from(value: CodeDatabaseError) -> Self {
        ResolverError::CodeDatabaseError(value)
    }
}

impl From<SourceHasherError> for ResolverError {
    fn from(value: SourceHasherError) -> Self {
        ResolverError::SourceHasherError(value)
    }
}

impl From<CodeManagerError> for ResolverError {
    fn from(value: CodeManagerError) -> Self {
        ResolverError::CodeManagerError(value)
    }
}
