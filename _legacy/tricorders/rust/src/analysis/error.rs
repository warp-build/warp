use std::path::PathBuf;
use thiserror::Error;

use super::cargo::CargoAnalyzerError;

#[derive(Error, Debug)]
pub enum GenerateSignatureError {
    #[error("Could not load file at {file:?} due to {err:?}")]
    CouldNotReadFile { file: String, err: std::io::Error },

    #[error("Missing dep {dep:?}")]
    MissingDependency { dep: String },

    #[error("Unsupported file {file:?}")]
    UnsupportedFile { file: PathBuf },

    #[error(transparent)]
    CargoAnalyzerError(CargoAnalyzerError),
}

impl From<CargoAnalyzerError> for GenerateSignatureError {
    fn from(value: CargoAnalyzerError) -> Self {
        GenerateSignatureError::CargoAnalyzerError(value)
    }
}
