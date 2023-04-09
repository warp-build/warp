pub mod ast;
mod dependency;
pub mod generate_signature;
pub mod model;
pub mod tree_splitter;

use self::ast::{GetAst, GetAstError};
use self::generate_signature::{GenerateSignature, GenerateSignatureError};
use self::model::{Ast, Signature, Symbol};
use std::path::Path;

#[derive(Default)]
pub struct Analysis {}

impl Analysis {
    pub async fn get_ast(file: String, symbol: Symbol) -> Result<Ast, GetAstError> {
        GetAst::get_ast(file, symbol).await
    }

    pub async fn generate_signature(
        workspace_root: String,
        file: String,
        test_matcher: Vec<String>,
    ) -> Result<Vec<Signature>, GenerateSignatureError> {
        println!("Analyzing: {:?}", &file);

        if !Self::is_supported_file(&file) {
            return Err(GenerateSignatureError::UnsupportedFile { file });
        }

        if !test_matcher.is_empty() {
            return GenerateSignature::test(workspace_root, file, test_matcher).await;
        }

        GenerateSignature::all(workspace_root, file).await
    }

    pub fn is_supported_file(file: &str) -> bool {
        match Path::new(&file).extension() {
            Some(ext) => ext == "rs",
            _ => false,
        }
    }
}
