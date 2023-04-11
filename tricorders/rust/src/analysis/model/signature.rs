use super::{Config, Requirement};
use std::path::PathBuf;
use thiserror::*;

#[derive(Builder, Debug, Clone)]
#[builder(build_fn(error = "SignatureError"))]
pub struct Signature {
    #[builder(setter(into))]
    target: String,

    #[builder(setter(into))]
    rule: String,

    #[builder(default)]
    deps: Vec<Requirement>,

    #[builder(default)]
    runtime_deps: Vec<Requirement>,

    #[builder(default)]
    config: Config,
}

impl Signature {
    pub fn builder() -> SignatureBuilder {
        Default::default()
    }

    pub fn set_target(&mut self, target: String) {
        self.target = target;
    }

    pub fn target(&self) -> &str {
        self.target.as_ref()
    }

    pub fn rule(&self) -> &str {
        self.rule.as_ref()
    }

    pub fn deps(&self) -> &[Requirement] {
        self.deps.as_ref()
    }

    pub fn runtime_deps(&self) -> &[Requirement] {
        self.runtime_deps.as_ref()
    }

    pub fn config(&self) -> &Config {
        &self.config
    }
}

impl std::fmt::Display for Signature {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(fmt, "{}(\"{}\")", self.rule, self.target)
    }
}
#[derive(Error, Debug)]
pub enum SignatureError {
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

impl Default for Signature {
    fn default() -> Self {
        Self {
            target: "".to_string(),
            rule: "".to_string(),
            deps: vec![],
            runtime_deps: vec![],
            config: Config::default(),
        }
    }
}
