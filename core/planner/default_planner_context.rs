use crate::resolver::TargetRegistry;
use crate::rules::{RuleStore, SharedJsContext};
use crate::store::DefaultStore;
use crate::sync::*;
use crate::worker::TaskResults;

#[derive(Debug, Clone)]
pub struct DefaultPlannerContext {
    pub(crate) artifact_store: Arc<DefaultStore>,
    pub(crate) target_registry: Arc<TargetRegistry>,
    pub(crate) task_results: Arc<TaskResults>,
    pub(crate) rule_store: Arc<RuleStore>,
}

impl DefaultPlannerContext {
    pub fn new(
        artifact_store: Arc<DefaultStore>,
        target_registry: Arc<TargetRegistry>,
        task_results: Arc<TaskResults>,
        rule_store: Arc<RuleStore>,
    ) -> Self {
        Self {
            artifact_store,
            target_registry,
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
