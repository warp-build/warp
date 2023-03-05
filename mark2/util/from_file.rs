use async_trait::async_trait;
use serde::Deserialize;
use std::path::{Path, PathBuf};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum FromFileError {
    #[error("Could not open file at {file:?} due to {err:?}")]
    CouldNotOpenFile { err: std::io::Error, file: PathBuf },

    #[error("Could not read file at {file:?} due to {err:?}")]
    CouldNotReadFile { err: std::io::Error, file: PathBuf },

    #[error("Could not parse file file at {file:?} due to {err:?}")]
    ParseError {
        err: serde_json::Error,
        file: PathBuf,
        bytes: String,
    },
}

#[async_trait]
pub trait FromFile<'a> {
    async fn from_file(path: &Path) -> Result<Self, FromFileError>
    where
        Self: Sized + Deserialize<'a>,
    {
        /*
        let mut file =
            tokio::fs::File::open(&path)
                .await
                .map_err(|err| FromFileError::CouldNotOpenFile {
                    err,
                    file: path.to_path_buf(),
                })?;

        let mut bytes = vec![];
        file.read_to_end(&mut bytes)
            .await
            .map_err(|err| FromFileError::CouldNotReadFile {
                err,
                file: path.to_path_buf(),
            })?;

        serde_json::from_slice(&bytes).map_err(|err| FromFileError::ParseError {
            err,
            file: path.to_path_buf(),
            bytes: String::from_utf8_lossy(&bytes).to_string(),
        })
        */
        unimplemented!()
    }
}
