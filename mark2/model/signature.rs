use super::{ConcreteTarget, Config, RuleName, Target};
use std::path::PathBuf;
use thiserror::*;

#[derive(Builder, Debug, Clone)]
pub struct Signature {
    target: ConcreteTarget,

    rule: RuleName,

    #[builder(default)]
    deps: Vec<Target>,

    #[builder(default)]
    runtime_deps: Vec<Target>,

    #[builder(default)]
    config: Config,
}

impl Signature {
    pub fn builder() -> SignatureBuilder {
        Default::default()
    }

    pub fn target(&self) -> &ConcreteTarget {
        &self.target
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
        write!(fmt, "{}(\"{}\")", self.rule, self.target.to_string())
    }
}
#[derive(Error, Debug)]
pub enum SignatureError {
    #[error("Could not parse JSON into Signatures: {0:?}")]
    ParseError(serde_json::Error),

    #[error("Could not print Signatures into JSON: {0:#?}")]
    PrintError(serde_json::Error),

    #[error("Could not open file at {file:?} due to {err:?}")]
    FileOpenError { file: PathBuf, err: std::io::Error },
}
