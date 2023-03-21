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
    pub async fn all(
        workspace_root: String,
        file: String,
    ) -> Result<Vec<Signature>, GenerateSignatureError> {
        let mut path = PathBuf::from(&workspace_root);
        path.push(&file);

        let file_name = Path::new(&file).file_name().unwrap().to_str().unwrap();
        let mut mod_paths: Vec<String> = vec![file_name.to_string()];
        // First we need to read the file contents
        let source = fs::read_to_string(path.canonicalize().unwrap())
            .await
            .map_err(|err| GenerateSignatureError::CouldNotReadFile {
                file: file.clone(),
                err,
            })
            .unwrap();

        // With the file contents, we try to find anything external to the file
        // that we might need. That is, either external modules declared with `mod`
        // or external crates that the file needs. For imported mods, we need
        // to also figure out if they reside in <mod_name>.rs or <mod_name>/mod.rs
        let (mods, _crates) = TreeSplitter::get_deps_all(&source);

        let file_path = PathBuf::from(
            path.canonicalize()
                .unwrap()
                .parent()
                .unwrap_or(Path::new("")),
        )
        .join("");
        match Self::find_mod_files(mods, file_path) {
            Ok(files) => files
                .iter()
                .map(|e| mod_paths.push(e.to_string()))
                .collect(),
            Err(err) => return Err(err),
        }

        let mut config = Config::default();
        config.insert(
            "srcs".to_string(),
            crate::Value::List(mod_paths.iter().map(|e| e.to_string().into()).collect()),
        );

        let signature = Signature::builder()
            .target(file)
            .rule("rust_binary".to_string())
            .config(config)
            .build()
            .unwrap();

        println!("{:#?}", signature.clone());
        Ok(vec![signature])
    }

    pub fn find_mod_files(
        mods: Vec<String>,
        path: PathBuf,
    ) -> Result<Vec<String>, GenerateSignatureError> {
        let mut mod_files: Vec<String> = Vec::new();
        for m in mods.iter() {
            let path1 = path.canonicalize().unwrap().join(format!("{}.rs", m));
            let path2 = path.join(format!("mod/{}.rs", m));
            if path1.exists() {
                mod_files.push(format!("{}.rs", m))
            } else if path2.exists() {
                mod_files.push(format!("mod/{}.rs", m))
            } else {
                return Err(GenerateSignatureError::MissingDependency { dep: m.to_string() });
            }
        }
        Ok(mod_files)
    }
}
