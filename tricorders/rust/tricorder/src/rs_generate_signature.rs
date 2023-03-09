use crate::proto::build::warp::Signature;
use std::path::PathBuf;

#[derive(Default)]
pub struct RsGenerateSignature {}

impl RsGenerateSignature {
    pub fn generate(_file: &str, _code_paths: Vec<PathBuf>) -> Vec<Signature> {
        let signatures = vec![];
        signatures
    }
}
