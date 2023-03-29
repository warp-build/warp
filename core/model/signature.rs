use super::rule::Config;
use super::{ConcreteTarget, RuleName, Task};
use serde::{Deserialize, Serialize};
use std::hash::Hash;
use std::path::PathBuf;
use thiserror::*;

#[derive(Builder, Debug, Clone, Serialize, Deserialize, Hash, Eq, PartialEq)]
#[builder(build_fn(error = "SignatureError"))]
pub struct Signature {
    #[builder(setter(into))]
    name: String,

    target: ConcreteTarget,

    #[builder(setter(into))]
    rule: RuleName,

    #[builder(default, setter(into))]
    deps: Vec<Task>,

    #[builder(default, setter(into))]
    runtime_deps: Vec<Task>,

    #[builder(default)]
    config: Config,
}

impl Signature {
    pub fn builder() -> SignatureBuilder {
        Default::default()
    }

    pub fn set_target(&mut self, target: ConcreteTarget) {
        self.target = target;
    }

    pub fn target(&self) -> &ConcreteTarget {
        &self.target
    }

    pub fn rule(&self) -> &str {
        self.rule.as_ref()
    }

    pub fn deps(&self) -> &[Task] {
        self.deps.as_ref()
    }

    pub fn runtime_deps(&self) -> &[Task] {
        self.runtime_deps.as_ref()
    }

    pub fn config(&self) -> &Config {
        &self.config
    }

    pub fn name(&self) -> &str {
        self.name.as_ref()
    }
}

impl AsRef<RuleName> for Signature {
    fn as_ref(&self) -> &RuleName {
        &self.rule
    }
}

impl std::fmt::Display for Signature {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(
            fmt,
            "{}({}, {})",
            self.rule,
            self.target.to_string(),
            self.name
        )
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

    #[error(transparent)]
    BuilderError(derive_builder::UninitializedFieldError),
}

impl From<derive_builder::UninitializedFieldError> for SignatureError {
    fn from(value: derive_builder::UninitializedFieldError) -> Self {
        SignatureError::BuilderError(value)
    }
}
