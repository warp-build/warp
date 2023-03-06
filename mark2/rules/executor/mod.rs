//! A Rule Executor that executes JavaScript rules using the Deno runtime.

mod js;
mod net_module_loader;

pub use js::*;

use crate::model::{Dependencies, Signature};
use crate::worker::ExecutionEnvironment;
use futures::Future;
use std::pin::Pin;
use thiserror::Error;

/// NOTE(@ostera): because the RuleExecutor uses Deno in one of its implementations, we can't use the #[async_trait] macro. This macro automatically marks all futures as +Send, which forces
/// Send on this RuleExecutor, which forces Send on the Deno instances.
///
pub trait RuleExecutor {
    type Context: Sync + Send + Clone + Sized;

    fn new(ctx: Self::Context) -> Result<Self, RuleExecutorError>
    where
        Self: Sized;

    fn execute<'a>(
        &'a mut self,
        env: &ExecutionEnvironment,
        sig: &Signature,
        deps: &Dependencies,
    ) -> Pin<Box<dyn Future<Output = Result<(), RuleExecutorError>> + 'a>>;
}

#[derive(Error, Debug)]
pub enum RuleExecutorError {}
