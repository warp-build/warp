use super::generate_signature::GenerateSignatureError;
use super::model::Signature;
use std::path::Path;

#[derive(Default)]
pub struct CargoAnalyzer;

impl CargoAnalyzer {
    pub async fn generate_signature(
        &self,
        _workspace_root: String,
        _file: &Path,
    ) -> Result<Vec<Signature>, GenerateSignatureError> {
        Ok(vec![])
    }
}
