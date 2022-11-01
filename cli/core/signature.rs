use std::path::{Path, PathBuf};

use super::*;
use serde::{Deserialize, Serialize};
use thiserror::*;

pub const BUILDFILE: &str = "Build.json";

#[derive(Error, Debug)]
pub enum SignatureError {
    #[error("Could not parse JSON into Signatures: {0:?}")]
    ParseError(serde_json::Error),

    #[error("Could not print Signatures into JSON: {0:#?}")]
    PrintError(serde_json::Error),
}

#[derive(Builder, Debug, Clone, Serialize, Deserialize)]
pub struct Signature {
    pub name: Label,

    pub rule: RuleName,

    #[serde(default)]
    pub deps: Vec<Label>,

    #[serde(default)]
    pub runtime_deps: Vec<Label>,

    #[serde(flatten)]
    pub config: RuleConfig,
}

impl AsRef<RuleName> for Signature {
    fn as_ref(&self) -> &RuleName {
        &self.rule
    }
}

impl std::fmt::Display for Signature {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(fmt, "{}(name = \"{}\")", self.rule, self.name.to_string())
    }
}

impl From<Signature> for Target {
    fn from(mut sig: Signature) -> Self {
        sig.config
            .insert("name".to_string(), CfgValue::Label(sig.name.clone()));

        sig.config.insert(
            "runtime_deps".to_string(),
            CfgValue::List(
                sig.runtime_deps
                    .iter()
                    .map(|l| CfgValue::Label(l.clone()))
                    .collect(),
            ),
        );

        sig.config.insert(
            "deps".to_string(),
            CfgValue::List(
                sig.deps
                    .iter()
                    .map(|l| CfgValue::Label(l.clone()))
                    .collect(),
            ),
        );

        Target::new(sig.name, &sig.rule, sig.config)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GeneratedSignature {
    pub version: usize,

    #[serde(default)]
    pub signatures: Vec<Signature>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SignaturesFile {
    #[serde(default)]
    pub signatures: Vec<Signature>,
}

impl SignaturesFile {
    pub async fn read<P1, P2>(file: P1, workspace_root: P2) -> Result<Self, SignatureError>
    where
        P1: AsRef<Path>,
        P2: AsRef<Path> + Clone,
    {
        let workspace_root = workspace_root.as_ref();
        let file = file.as_ref();
        let package_path = file
            .parent()
            .unwrap()
            .strip_prefix(&workspace_root)
            .unwrap();

        let file = tokio::fs::File::open(&file).await.unwrap();
        let buffer = std::io::BufReader::new(file.into_std().await);
        let reader = json_comments::StripComments::new(buffer);
        let mut sig_file: Self =
            serde_json::from_reader(reader).map_err(SignatureError::ParseError)?;

        for sig in &mut sig_file.signatures {
            sig.name = {
                let mut name = sig.name.to_abstract().unwrap();
                name.set_workspace(&workspace_root);
                name.set_path(&package_path);
                name
            };
            for dep in sig.deps.iter_mut().chain(sig.runtime_deps.iter_mut()) {
                dep.set_workspace(&workspace_root);
            }
        }

        Ok(sig_file)
    }
}