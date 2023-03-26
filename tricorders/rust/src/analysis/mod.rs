mod generate_signature;
pub use generate_signature::*;

use crate::models::Signature;
use std::path::Path;

#[derive(Default)]
pub struct Analysis {}

impl Analysis {
    pub async fn generate_signature(
        workspace_root: String,
        file: String,
        test_matcher: Vec<String>,
    ) -> Result<Vec<Signature>, GenerateSignatureError> {
        println!("Analyzing: {:?}", &file);

        if !Self::is_supported_file(&file) {
            return Err(GenerateSignatureError::UnsupportedFile { file });
        }

        if test_matcher.len() != 0 {
            return GenerateSignature::test(workspace_root, file, test_matcher).await;
        }

        let sigs = GenerateSignature::all(workspace_root, file).await;
        if let Err(err) = sigs {
            return Err(err);
        }
        Ok(sigs.unwrap())
    }

    pub fn is_supported_file(file: &str) -> bool {
        match Path::new(&file).extension() {
            Some(ext) => ext == "rs",
            _ => false,
        }
    }
}
