pub mod ast;
mod cargo;
mod dependency;
pub mod generate_signature;
pub mod model;
mod rust_module;
pub mod tree_splitter;

use self::ast::{GetAst, GetAstError};
use self::cargo::CargoAnalyzer;
use self::generate_signature::GenerateSignatureError;
use self::model::{Ast, Signature, Symbol};
use self::rust_module::RustModuleAnalyzer;
use std::path::Path;

#[derive(Default)]
pub struct Analyzer {
    cargo_analyzer: CargoAnalyzer,
    rust_module_analyzer: RustModuleAnalyzer,
}

impl Analyzer {
    pub async fn get_ast(&self, file: String, symbol: Symbol) -> Result<Ast, GetAstError> {
        GetAst::get_ast(file, symbol).await
    }

    pub async fn generate_signature(
        &self,
        workspace_root: String,
        file: &Path,
        test_matcher: Vec<String>,
    ) -> Result<Vec<Signature>, GenerateSignatureError> {
        println!("Analyzing: {:?}", &file);

        let ext = file.extension().unwrap().to_str().unwrap();
        let basename = file.file_name().unwrap().to_str().unwrap();
        match (basename, ext) {
            ("Cargo.toml", _) => {
                self.cargo_analyzer
                    .generate_signature(workspace_root, file)
                    .await
            }
            (_, "rs") => {
                self.rust_module_analyzer
                    .generate_signature(workspace_root, file, test_matcher)
                    .await
            }
            _ => Err(GenerateSignatureError::UnsupportedFile {
                file: file.to_path_buf(),
            }),
        }
    }
}
