use crate::dependencies::DependencyManager;

use super::ast::{GetAst, GetAstError};
use super::cargo::CargoAnalyzer;
use super::model::{Ast, Signature, Symbol};
use super::rust_module::RustModuleAnalyzer;
use super::GenerateSignatureError;
use std::path::Path;
use std::sync::Arc;

#[derive(Default)]
pub struct Analyzer {
    cargo_analyzer: CargoAnalyzer,
    rust_module_analyzer: RustModuleAnalyzer,
}

impl Analyzer {
    pub fn new(dep_manager: Arc<DependencyManager>) -> Self {
        let cargo_analyzer = CargoAnalyzer::new(dep_manager);
        let rust_module_analyzer = RustModuleAnalyzer::default();

        Self {
            cargo_analyzer,
            rust_module_analyzer,
        }
    }

    pub async fn get_ast(&self, file: String, symbol: Symbol) -> Result<Ast, GetAstError> {
        GetAst::get_ast(file, symbol).await
    }

    pub async fn generate_signature(
        &self,
        workspace_root: &Path,
        file: &Path,
        test_matcher: Vec<String>,
    ) -> Result<Vec<Signature>, GenerateSignatureError> {
        println!("Analyzing: {:?}", &file);

        let ext = file.extension().unwrap().to_str().unwrap();
        let basename = file.file_name().unwrap().to_str().unwrap();
        match (basename, ext) {
            ("Cargo.toml", _) => {
                let signatures = self
                    .cargo_analyzer
                    .generate_signature(workspace_root, file)
                    .await?;
                Ok(signatures)
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
