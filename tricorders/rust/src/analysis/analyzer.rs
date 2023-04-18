use crate::dependencies::crates::Crates;
use crate::dependencies::DependencyManager;

use super::ast::{GetAst, GetAstError};
use super::cargo::CargoAnalyzer;
use super::model::{Ast, Goal, Signature};
use super::rust_module::RustModuleAnalyzer;
use super::GenerateSignatureError;
use std::path::Path;
use std::sync::Arc;

pub struct Analyzer {
    cargo_analyzer: CargoAnalyzer,
    rust_module_analyzer: RustModuleAnalyzer,
}

impl Analyzer {
    pub fn new(dep_manager: Arc<DependencyManager>) -> Self {
        let crates = Arc::new(Crates::new());
        let cargo_analyzer = CargoAnalyzer::new(crates, dep_manager);
        let rust_module_analyzer = RustModuleAnalyzer::default();

        Self {
            cargo_analyzer,
            rust_module_analyzer,
        }
    }

    pub async fn get_ast(
        &self,
        workspace_root: &Path,
        file: &Path,
        test_matcher: Vec<String>,
    ) -> Result<Vec<Ast>, GetAstError> {
        GetAst::get_ast(&workspace_root.join(file), test_matcher).await
    }

    pub async fn generate_signature(
        &self,
        goal: Goal,
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

                println!("Generated {} signatures", signatures.len());

                Ok(signatures)
            }
            (_, "rs") => {
                let signatures = self
                    .rust_module_analyzer
                    .generate_signature(goal, workspace_root, file, test_matcher)
                    .await?;

                println!("Generated {} signatures", signatures.len());

                Ok(signatures)
            }
            _ => Err(GenerateSignatureError::UnsupportedFile {
                file: file.to_path_buf(),
            }),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use crate::analysis::cargo::RULE_RUST_BINARY;
    use crate::analysis::model::Requirement;

    use super::*;
    use assert_fs::prelude::*;

    #[tokio::test]
    async fn generates_cargo_toml_signatures() {
        let workspace_root = assert_fs::TempDir::new().unwrap();
        dbg!(&workspace_root);

        let cargo_toml = workspace_root.child("Cargo.toml");
        cargo_toml
            .write_str(
                r#"
[package]
name = "test"
version = "0.1.0"

[[bin]]
name = "test"
path = "src/main.rs"

[dependencies]
anyhow = "1"
            "#,
            )
            .unwrap();

        workspace_root
            .child("Cargo.lock")
            .write_str(
                r#"
# This file is automatically @generated by Cargo.
# It is not intended for manual editing.
version = 3

[[package]]
name = "anyhow"
version = "1.0.68"
source = "registry+https://github.com/rust-lang/crates.io-index"
checksum = "2cb2f989d18dd141ab8ae82f64d1a8cdd37e0840f73a406896cf5e99502fab61"
            "#,
            )
            .unwrap();

        let dep_manager = Arc::new(DependencyManager::new(workspace_root.path()));
        dep_manager.prepare().await.unwrap();

        let analyzer = Analyzer::new(dep_manager);

        let signatures = analyzer
            .generate_signature(
                Goal::Build,
                workspace_root.path(),
                cargo_toml.path(),
                vec![],
            )
            .await
            .unwrap();

        assert_eq!(signatures.len(), 1);

        let test_bin = signatures.get(0).unwrap();

        assert_eq!(test_bin.name(), "test");
        assert_eq!(test_bin.rule(), RULE_RUST_BINARY);

        assert_eq!(test_bin.deps().len(), 1);
        let dep = test_bin.deps().get(0).unwrap();

        dbg!(&dep);
        assert_matches!(dep, Requirement::Remote { url, subpath, .. } => {
            assert_eq!(url.to_string(), "https://crates.io/api/v1/crates/anyhow/1.0.68/download".to_string());
            assert_eq!(*subpath, PathBuf::from("anyhow-1.0.68"));
        });
    }
}
