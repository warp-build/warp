use crate::models::{Config, Signature};
use crate::tree_splitter::TreeSplitter;
use std::path::{Path, PathBuf};
use thiserror::*;
use tokio::fs;

#[derive(Default)]
pub struct GenerateSignature {}

#[derive(Error, Debug)]
pub enum GenerateSignatureError {
    #[error("Could not load file at {file:?} due to {err:?}")]
    CouldNotReadFile { file: String, err: std::io::Error },

    #[error("Missing dep {dep:?}")]
    MissingDependency { dep: String },
}

impl GenerateSignature {
    pub async fn all(file: &str) -> Vec<Signature> {
        // First we need to read the file contents
        let source = fs::read_to_string(&file)
            .await
            .map_err(|err| GenerateSignatureError::CouldNotReadFile {
                file: file.to_string(),
                err,
            })
            .unwrap();

        // With the file contents, we try to find anything external to the file
        // that we might need. That is, either external modules declared with `mod`
        // or external crates that the file needs. For imported mods, we need
        // to also figure out if they reside in <mod_name>.rs or <mod_name>/mod.rs
        let (mods, _crates) = TreeSplitter::get_deps_all(&source);

        let path = PathBuf::from(Path::new(file).parent().unwrap_or(Path::new(""))).join("");
        let mod_files: Vec<String> = Self::find_mod_files(mods, path).unwrap();

        let mut config = Config::default();
        config.insert(
            "srcs".to_string(),
            crate::Value::List(mod_files.iter().map(|e| e.to_string().into()).collect()),
        );

        let signature = Signature::builder()
            .target(file.to_string())
            .rule("rust_binary".to_string())
            .config(config)
            .build();

        vec![signature.unwrap()]
    }

    pub fn find_mod_files(
        mods: Vec<String>,
        path: PathBuf,
    ) -> Result<Vec<String>, GenerateSignatureError> {
        let mut mod_files: Vec<String> = Vec::new();
        for m in mods.iter() {
            let path1 = path.join(format!("{}.rs", m));
            let path2 = path.join(format!("mod/{}.rs", m));
            if path1.exists() {
                mod_files.push(path1.display().to_string())
            } else if path2.exists() {
                mod_files.push(path2.display().to_string())
            } else {
                return Err(GenerateSignatureError::MissingDependency { dep: m.to_string() });
            }
        }
        Ok(mod_files)
    }
}
