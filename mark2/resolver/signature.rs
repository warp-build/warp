use crate::rules::{config::Config, RuleName};
use serde::{Deserialize, Serialize};
use std::path::PathBuf;
use thiserror::*;

use super::Target;

pub const BUILDFILE: &str = "Build.json";

#[derive(Error, Debug)]
pub enum SignatureError {
    #[error("Could not parse JSON into Signatures: {0:?}")]
    ParseError(serde_json::Error),

    #[error("Could not print Signatures into JSON: {0:#?}")]
    PrintError(serde_json::Error),

    #[error("Could not open file at {file:?} due to {err:?}")]
    FileOpenError { file: PathBuf, err: std::io::Error },
}

#[derive(Builder, Debug, Clone, Serialize, Deserialize)]
pub struct Signature {
    name: Target,

    rule: RuleName,

    #[serde(default)]
    deps: Vec<Target>,

    #[serde(default)]
    runtime_deps: Vec<Target>,

    #[serde(flatten)]
    config: Config,
}

impl Signature {
    pub fn name(&self) -> &Target {
        &self.name
    }

    pub fn rule(&self) -> &str {
        self.rule.as_ref()
    }

    pub fn deps(&self) -> &[Target] {
        self.deps.as_ref()
    }

    pub fn runtime_deps(&self) -> &[Target] {
        self.runtime_deps.as_ref()
    }

    pub fn config(&self) -> &Config {
        &self.config
    }
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

/// This is the schema for a signature generated on-demand.
///
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GeneratedSignature {
    #[serde(default)]
    pub signatures: Vec<Signature>,
}

/// This is the schema for a Build.json file.
///
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SignaturesFile {
    #[serde(default)]
    pub signatures: Vec<Signature>,
}
