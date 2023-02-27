use std::path::{Path, PathBuf};

use async_trait::async_trait;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum FromFileError {
    #[error("Could not open file at {file:?} due to {err:?}")]
    CouldNotOpenFile { err: std::io::Error, file: PathBuf },

    #[error("Could not read file at {file:?} due to {err:?}")]
    CouldNotReadFile { err: std::io::Error, file: PathBuf },
}

#[async_trait]
pub trait FromFile {
    async fn from_file(file: &Path) -> Result<Self, FromFileError> {
        let mut file =
            tokio::fs::File::open(file)
                .await
                .map_err(|err| FromFileError::CouldNotOpenFile {
                    err,
                    file: file.to_path_buf(),
                })?;

        let mut bytes = vec![];
        file.read_to_end(&mut bytes)
            .await
            .map_err(|err| FromFileError::CouldNotReadFile {
                err,
                file: file.to_path_buf(),
            })?;

        let data = serde_json::from_slice(&bytes)?;
        Ok(data)
    }
}
