use super::generate_signature::{GenerateSignature, GenerateSignatureError};
use super::model::Signature;
use std::path::Path;

#[derive(Default)]
pub struct RustModuleAnalyzer;

impl RustModuleAnalyzer {
    pub async fn generate_signature(
        &self,
        workspace_root: String,
        file: &Path,
        test_matcher: Vec<String>,
    ) -> Result<Vec<Signature>, GenerateSignatureError> {
        let file = file.to_string_lossy().to_string();

        if !test_matcher.is_empty() {
            return GenerateSignature::test(workspace_root, file, test_matcher).await;
        }

        GenerateSignature::all(workspace_root, file).await
    }
}
