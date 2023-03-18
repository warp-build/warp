use crate::proto::build::warp::Signature;
use crate::proto::google::protobuf::{value::Kind, ListValue, Struct, Value};
use crate::tree_splitter::TreeSplitter;
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use thiserror::*;
use tokio::fs;

#[derive(Default)]
pub struct RsGenerateSignature {}

#[derive(Error, Debug)]
pub enum RsGenerateSignatureError {
    #[error("Could not load file at {file:?} due to {err:?}")]
    CouldNotReadFile { file: String, err: std::io::Error },

    #[error("Missing dep {dep:?}")]
    MissingDependency { dep: String },
}

impl RsGenerateSignature {
    pub async fn generate_all(file: &str, _code_paths: Vec<PathBuf>) -> Vec<Signature> {
        // First we need to read the file contents
        let source = fs::read_to_string(&file)
            .await
            .map_err(|err| RsGenerateSignatureError::CouldNotReadFile {
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
        let mod_files = Self::find_mod_files(mods, path).unwrap();

        let signature = Signature {
            name: file.to_string(),
            deps: vec![],
            rule: "rust_binary".to_string(),
            runtime_deps: vec![],
            config: Some(Struct {
                fields: HashMap::from_iter(vec![(
                    "srcs".to_string(),
                    Value {
                        kind: Some(Kind::ListValue(ListValue {
                            values: mod_files
                                .iter()
                                .map(|s| Value {
                                    kind: Some(Kind::StringValue(s.to_string())),
                                })
                                .collect(),
                        })),
                    },
                )]),
            }),
        };
        vec![signature]
    }

    pub fn find_mod_files(
        mods: Vec<String>,
        path: PathBuf,
    ) -> Result<Vec<String>, RsGenerateSignatureError> {
        let mut mod_files: Vec<String> = Vec::new();
        for m in mods.iter() {
            let path1 = path.join(format!("{}.rs", m));
            let path2 = path.join(format!("mod/{}.rs", m));
            if path1.exists() {
                mod_files.push(path1.display().to_string())
            } else if path2.exists() {
                mod_files.push(path2.display().to_string())
            } else {
                return Err(RsGenerateSignatureError::MissingDependency { dep: m.to_string() });
            }
        }
        Ok(mod_files)
    }
}
