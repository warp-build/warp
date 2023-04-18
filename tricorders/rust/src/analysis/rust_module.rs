use super::generate_signature::GenerateSignature;
use super::model::{Goal, Signature};
use super::GenerateSignatureError;
use std::path::Path;

#[derive(Default)]
pub struct RustModuleAnalyzer;

impl RustModuleAnalyzer {
    pub async fn generate_signature(
        &self,
        goal: Goal,
        workspace_root: &Path,
        file: &Path,
        test_matcher: Vec<String>,
    ) -> Result<Vec<Signature>, GenerateSignatureError> {
        match goal {
            Goal::Build => GenerateSignature::build(workspace_root, file).await,
            Goal::Test => GenerateSignature::test(workspace_root, file, test_matcher).await,
            Goal::Run => todo!(),
        }
    }
}
