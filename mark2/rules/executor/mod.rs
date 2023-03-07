//! A Rule Executor that executes JavaScript rules using the Deno runtime.

mod compute_script;
mod context;
mod js;
mod net_module_loader;
mod result;

use compute_script::*;
pub use context::*;
use dashmap::DashMap;
pub use js::*;
pub use result::*;

use crate::model::rule::expander::ExpanderError;
use crate::model::{Dependencies, Rule, Signature, Target, TargetId};
use crate::worker::ExecutionEnvironment;
use futures::Future;
use std::path::PathBuf;
use std::pin::Pin;
use thiserror::Error;

/// NOTE(@ostera): because the RuleExecutor uses Deno in one of its implementations, we can't use
/// the #[async_trait] macro. This macro automatically marks all futures as +Send, which forces
/// Send on this RuleExecutor, which forces Send on the Deno instances.
///
pub trait RuleExecutor {
    type Context: Sync + Send + Clone + Sized;

    fn new(ctx: Self::Context) -> Result<Self, RuleExecutorError>
    where
        Self: Sized;

    fn execute<'a>(
        &'a mut self,
        env: &'a ExecutionEnvironment,
        sig: &'a Signature,
        deps: &'a Dependencies,
    ) -> Pin<Box<dyn Future<Output = Result<ExecutionResult, RuleExecutorError>> + 'a>>;
}

#[derive(Error, Debug)]
pub enum RuleExecutorError {
    #[error(transparent)]
    DenoExecutionError(anyhow::Error),

    #[error(transparent)]
    ConfigExpanderError(ExpanderError),

    #[error("Could not find declared outputs for target {target:?} in outputs: {outputs:#?}")]
    MissingDeclaredOutputs {
        target: TargetId,
        outputs: DashMap<TargetId, Vec<PathBuf>>,
    },

    #[error("Execution Error for {}\nSignature: {sig:#?}\n\nError: {err:?}", target.to_string())]
    ExecutionError {
        err: anyhow::Error,
        target: Target,
        sig: Box<Signature>,
        rule: Rule,
    },
}
