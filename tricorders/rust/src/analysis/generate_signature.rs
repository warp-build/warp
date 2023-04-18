use super::model::{Config, Signature, Value};
use super::tree_splitter::TreeSplitter;
use super::GenerateSignatureError;
use std::path::{Path, PathBuf};

use tokio::fs;

#[derive(Default)]
pub struct GenerateSignature {}

impl GenerateSignature {
    pub async fn build(
        workspace_root: &Path,
        file: &Path,
    ) -> Result<Vec<Signature>, GenerateSignatureError> {
        // NOTE(@Capitanu): With the file contents, we try to find anything external to the file
        // that we might need. That is, either external modules declared with `mod`
        // or external crates that the file needs. For imported mods, we need
        // to also figure out if they reside in <mod_name>.rs or <mod_name>/mod.rs
        let (mods, _crates) = {
            let path = Path::new(&workspace_root).join(file);
            // First we need to read the file contents
            let source = fs::read_to_string(&path)
                .await
                .map_err(|err| GenerateSignatureError::CouldNotReadFile {
                    file: file.display().to_string(),
                    err,
                })
                .unwrap();
            TreeSplitter::get_deps_all(&source)
        };

        let file_path = workspace_root.join(file);
        let file_dir = file_path.parent().unwrap();
        let mut mod_paths = vec![PathBuf::from(
            file_path.file_name().unwrap().to_str().unwrap(),
        )];
        let mod_files = Self::find_mod_files(mods, file_dir).await?;
        mod_paths.extend(mod_files);

        let mut config = Config::default();
        config.insert(
            "srcs".to_string(),
            Value::List(
                mod_paths
                    .iter()
                    .map(|e| e.to_string_lossy().to_string().into())
                    .collect(),
            ),
        );

        let signature = Signature::builder()
            .name(file.to_string_lossy().to_string())
            .rule("rust_test".to_string())
            .config(config)
            .build()
            .unwrap();

        Ok(vec![signature])
    }

    pub async fn test(
        workspace_root: &Path,
        file: &Path,
        test_matcher: Vec<String>,
    ) -> Result<Vec<Signature>, GenerateSignatureError> {
        let path = workspace_root.join(file);
        let file_name = path.file_name().unwrap().to_str().unwrap();
        let mut mod_paths: Vec<PathBuf> = vec![PathBuf::from(file_name.to_string())];
        // First we need to read the file contents

        let source = TreeSplitter::parse_file(path.clone());
        // With the file contents, we try to find anything external to the file
        // that we might need. That is, either external modules declared with `mod`
        // or external crates that the file needs. For imported mods, we need
        // to also figure out if they reside in <mod_name>.rs or <mod_name>/mod.rs
        let (mods, _crates) = TreeSplitter::get_deps_all(&source);

        let file_path = path.parent().unwrap();
        let mod_files: Vec<PathBuf> = Self::find_mod_files(mods, file_path).await?;
        mod_paths.extend(mod_files.iter().cloned());

        let matching_tests = TreeSplitter::find_matching_tests(test_matcher, &source);

        let signatures: Vec<Signature> = matching_tests
            .iter()
            .map(|test_name| {
                let test_name = test_name.to_string();
                let iter_mods = mod_paths.clone();
                Self::compose_test_signature(test_name, iter_mods)
            })
            .collect();

        Ok(signatures)
    }

    fn compose_test_signature(test_name: String, mod_paths: Vec<PathBuf>) -> Signature {
        let mut config = Config::default();
        config.insert(
            "srcs".to_string(),
            Value::List(
                mod_paths
                    .iter()
                    .map(|e| e.display().to_string().into())
                    .collect(),
            ),
        );

        config.insert("test".to_string(), Value::String(test_name.clone()));

        Signature::builder()
            .name(test_name)
            .rule("rust_test".to_string())
            .config(config)
            .build()
            .unwrap()
    }

    pub async fn find_mod_files(
        mods: Vec<String>,
        path: &Path,
    ) -> Result<Vec<PathBuf>, GenerateSignatureError> {
        let mut mod_files: Vec<PathBuf> = Vec::new();
        for m in mods.iter() {
            let mod_dir = Path::new("mod");
            let path1 = path.join(Path::new(m).with_extension("rs"));
            let path2 = path.join(mod_dir.join(m).with_extension("rs"));
            if fs::metadata(&path1).await.is_ok() {
                mod_files.push(path1.file_name().unwrap().into());
                continue;
            }
            if fs::metadata(&path2).await.is_ok() {
                mod_files.push(path2.file_name().unwrap().into());
                continue;
            }
            return Err(GenerateSignatureError::MissingDependency { dep: m.to_string() });
        }
        Ok(mod_files)
    }
}
