use super::generate_signature::GenerateSignature;
use super::model::Signature;
use super::GenerateSignatureError;
use std::path::Path;

#[derive(Default)]
pub struct RustModuleAnalyzer;

impl RustModuleAnalyzer {
    pub async fn generate_signature(
        &self,
        workspace_root: &Path,
        file: &Path,
        test_matcher: Vec<String>,
    ) -> Result<Vec<Signature>, GenerateSignatureError> {
        let file = file.to_string_lossy().to_string();
        let workspace_root = workspace_root.to_string_lossy().to_string();

        if !test_matcher.is_empty() {
            return GenerateSignature::test(workspace_root, file, test_matcher).await;
        }

        GenerateSignature::all(workspace_root, file).await
    }
}
