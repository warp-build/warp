use thiserror::Error;

use crate::tricorder::{TricorderError, TricorderManagerError};

use super::{CodeDatabaseError, SourceHasherError};

#[derive(Error, Debug)]
pub enum CodeManagerError {
    #[error(transparent)]
    CodeDatabaseError(CodeDatabaseError),

    #[error(transparent)]
    TricorderManagerError(TricorderManagerError),

    #[error(transparent)]
    TricorderError(TricorderError),

    #[error(transparent)]
    SourceHasherError(SourceHasherError),
}

impl From<CodeDatabaseError> for CodeManagerError {
    fn from(value: CodeDatabaseError) -> Self {
        CodeManagerError::CodeDatabaseError(value)
    }
}

impl From<TricorderManagerError> for CodeManagerError {
    fn from(value: TricorderManagerError) -> Self {
        CodeManagerError::TricorderManagerError(value)
    }
}

impl From<TricorderError> for CodeManagerError {
    fn from(value: TricorderError) -> Self {
        CodeManagerError::TricorderError(value)
    }
}

impl From<SourceHasherError> for CodeManagerError {
    fn from(value: SourceHasherError) -> Self {
        CodeManagerError::SourceHasherError(value)
    }
}
