mod generate_signature;
pub use generate_signature::*;

use crate::models::{Signature, Symbol};
use std::path::Path;
use thiserror::*;

#[derive(Default)]
pub struct Analysis {}

#[derive(Error, Debug)]
pub enum AnalysisError {
    #[error("Could not load file at {file:?} due to {err:?}")]
    CouldNotReadFile { file: String, err: std::io::Error },

    #[error("Missing dep {dep:?}")]
    MissingDependency { dep: String },
}

impl Analysis {
    pub async fn generate_signature(
        workspace_root: String,
        file: String,
        symbol: Symbol,
    ) -> Result<Vec<Signature>, AnalysisError> {
        println!("Analyzing: {:?}", file.clone());

        match Path::new(&file).extension() {
            Some(ext) if ext == "rs" => match symbol.scope() {
                All => Ok(GenerateSignature::all(&file).await),
                Named => Ok(vec![Signature::default()]), // TODO(@calin): GenerateSignature::named().await,
            },
            _ => Ok(vec![Signature::default()]),
        }
    }
}
