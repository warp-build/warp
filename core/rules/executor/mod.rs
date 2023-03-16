//! A Rule Executor that executes JavaScript rules using the Deno runtime.

mod compute_script;
mod context;
mod js;
mod js_ffi;
mod net_module_loader;
mod result;

pub use context::*;
pub use js::*;
pub use result::*;

use crate::model::rule::expander::ExpanderError;
use crate::model::{Dependencies, ExecutionEnvironment, Rule, Signature, Target, TargetId};
use async_trait::async_trait;
use dashmap::DashMap;
use std::path::PathBuf;
use thiserror::Error;

use super::RuleStoreError;

#[async_trait(?Send)]
pub trait RuleExecutor {
    type Context: Sync + Send + Clone + Sized;

    fn new(ctx: Self::Context) -> Result<Self, RuleExecutorError>
    where
        Self: Sized;

    async fn execute(
        &mut self,
        env: &ExecutionEnvironment,
        sig: &Signature,
        deps: &Dependencies,
    ) -> Result<ExecutionResult, RuleExecutorError>;
}

#[derive(Error, Debug)]
pub enum RuleExecutorError {
    #[error(transparent)]
    DenoExecutionError(anyhow::Error),

    #[error(transparent)]
    ConfigExpanderError(ExpanderError),

    #[error(transparent)]
    RuleStoreError(RuleStoreError),

    #[error("Could not find declared outputs for target {target:?} in outputs: {outputs:#?}")]
    MissingDeclaredOutputs {
        target: TargetId,
        outputs: DashMap<TargetId, Vec<PathBuf>>,
    },

    #[error("Execution Error for {}\nSignature: {sig:#?}\n\nError: {err:?}", target.to_string())]
    ExecutionError {
        err: anyhow::Error,
        target: Box<Target>,
        sig: Box<Signature>,
        rule: Box<Rule>,
    },

    #[error("The module name `{module_name}` is invalid: {reason:?}")]
    BadModuleName {
        module_name: String,
        reason: deno_core::url::ParseError,
    },

    #[error("The module `{module_name}` had issues importing some other files: {reason:?}")]
    ModuleResolutionError {
        module_name: String,
        reason: anyhow::Error,
    },

    #[error("The module name `{module_name}` could not be evaluated: {reason:?}")]
    ModuleEvaluationError {
        module_name: String,
        reason: anyhow::Error,
    },

    #[error("Could not find rule {rule_name:?} in the rules map")]
    RuleNotFound { rule_name: String },

    #[error("Could not load file at {file:?} due to {err:?}")]
    CouldNotReadFile { file: PathBuf, err: std::io::Error },

    #[error(transparent)]
    RuleExpanderError(ExpanderError),
}

impl From<RuleStoreError> for RuleExecutorError {
    fn from(value: RuleStoreError) -> Self {
        RuleExecutorError::RuleStoreError(value)
    }
}

impl From<ExpanderError> for RuleExecutorError {
    fn from(value: ExpanderError) -> Self {
        RuleExecutorError::RuleExpanderError(value)
    }
}
