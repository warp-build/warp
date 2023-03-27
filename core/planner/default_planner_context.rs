use crate::resolver::{SignatureRegistry, TargetRegistry};
use crate::rules::{RuleStore, SharedJsContext};
use crate::sync::*;
use crate::worker::{TaskRegistry, TaskResults};

#[derive(Debug, Clone)]
pub struct DefaultPlannerContext {
    pub(crate) task_registry: Arc<TaskRegistry>,
    pub(crate) target_registry: Arc<TargetRegistry>,
    pub(crate) signature_registry: Arc<SignatureRegistry>,
    pub(crate) task_results: Arc<TaskResults>,
    pub(crate) rule_store: Arc<RuleStore>,
}

impl DefaultPlannerContext {
    pub fn new(
        task_registry: Arc<TaskRegistry>,
        target_registry: Arc<TargetRegistry>,
        signature_registry: Arc<SignatureRegistry>,
        task_results: Arc<TaskResults>,
        rule_store: Arc<RuleStore>,
    ) -> Self {
        Self {
            task_registry,
            target_registry,
            signature_registry,
            task_results,
            rule_store,
        }
    }
}

impl From<DefaultPlannerContext> for SharedJsContext {
    fn from(ctx: DefaultPlannerContext) -> Self {
        SharedJsContext::new(ctx.target_registry, ctx.task_results, ctx.rule_store)
    }
}

impl From<DefaultPlannerContext> for () {
    fn from(_value: DefaultPlannerContext) {}
}
