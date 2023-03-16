use sha2::{Digest, Sha256};
use std::fs::File;
use std::io::{BufReader, Read};
use std::path::Path;
use thiserror::*;

pub struct SourceHasher;

impl SourceHasher {
    pub async fn hash<P>(file: P) -> Result<String, SourceHasherError>
    where
        P: AsRef<Path>,
    {
        let f = File::open(file.as_ref())?;
        let mut s = Sha256::new();
        let mut buffer = [0; 2048];
        let mut reader = BufReader::new(f);
        while let Ok(len) = reader.read(&mut buffer) {
            if len == 0 {
                break;
            }
            s.update(&buffer[..len]);
        }
        Ok(format!("{:x}", s.finalize()))
    }
}

#[derive(Error, Debug)]
pub enum SourceHasherError {
    #[error("Source hasher could not open source file due to {err:?}")]
    CouldNotOpenSource { err: std::io::Error },
}

impl From<std::io::Error> for SourceHasherError {
    fn from(err: std::io::Error) -> Self {
        SourceHasherError::CouldNotOpenSource { err }
    }
}
