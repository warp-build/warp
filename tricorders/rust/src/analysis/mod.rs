mod generate_signature;
pub use generate_signature::*;

use crate::models::{Signature, Symbol};
use std::path::Path;

#[derive(Default)]
pub struct Analysis {}

impl Analysis {
    pub async fn generate_signature(
        workspace_root: String,
        file: String,
        symbol: Symbol,
    ) -> Result<Vec<Signature>, GenerateSignatureError> {
        println!("Analyzing: {:?}", &file);

        if !Self::is_supported_file(&file) {
            return Err(GenerateSignatureError::UnsupportedFile { file });
        }

        match symbol {
            crate::Symbol::All => {
                let sigs = GenerateSignature::all(workspace_root, file).await;
                if let Err(err) = sigs {
                    return Err(err);
                }
                Ok(sigs.unwrap())
            }
            crate::Symbol::Named { name: _ } => Ok(vec![Signature::default()]), // TODO(@calin): GenerateSignature::named().await,
        }
    }

    pub fn is_supported_file(file: &str) -> bool {
        match Path::new(&file).extension() {
            Some(ext) => ext == "rs",
            _ => false,
        }
    }
}
