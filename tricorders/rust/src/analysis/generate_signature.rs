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

    #[error("Unsupported file {file:?}")]
    UnsupportedFile { file: String },
}

impl GenerateSignature {
    pub async fn all(
        workspace_root: String,
        file: String,
    ) -> Result<Vec<Signature>, GenerateSignatureError> {
        let path = Path::new(&workspace_root).join(&file);

        let file_name = path.file_name().unwrap().to_str().unwrap();
        let mut mod_paths: Vec<PathBuf> = vec![PathBuf::from(file_name.to_string())];
        // First we need to read the file contents
        let source = fs::read_to_string(&path)
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
        let file_path = path.parent().unwrap();
        let mod_files: Vec<PathBuf> = Self::find_mod_files(mods, file_path).await?;
        mod_paths.extend(mod_files.iter().cloned());
        let mut config = Config::default();
        config.insert(
            "srcs".to_string(),
            crate::Value::List(
                mod_paths
                    .iter()
                    .map(|e| e.display().to_string().into())
                    .collect(),
            ),
        );

        let signature = Signature::builder()
            .target(file)
            .rule("rust_test".to_string())
            .config(config)
            .build()
            .unwrap();

        Ok(vec![signature])
    }

    pub async fn find_mod_files(
        mods: Vec<String>,
        path: &Path,
    ) -> Result<Vec<PathBuf>, GenerateSignatureError> {
        let mut mod_files: Vec<PathBuf> = Vec::new();
        for m in mods.iter() {
            let mod_dir = Path::new("mod");
            let path1 = path.join(Path::new(m).with_extension("rs").to_path_buf());
            let path2 = path.join(mod_dir.join(m).with_extension("rs").to_path_buf());
            if let Ok(_) = fs::metadata(&path1).await {
                mod_files.push(path1.file_name().unwrap().into());
                continue;
            }
            if let Ok(_) = fs::metadata(&path2).await {
                mod_files.push(path2.file_name().unwrap().into());
                continue;
            }
            return Err(GenerateSignatureError::MissingDependency { dep: m.to_string() });
        }
        Ok(mod_files)
    }
}
