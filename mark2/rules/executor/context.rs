use crate::model::{Rule, RunScript, TargetId};
use crate::sync::*;
use crate::worker::TaskResults;
use dashmap::DashMap;
use fxhash::FxHashMap;
use std::path::PathBuf;
use uuid::Uuid;

// TODO(@ostera): remove when we bring in the executor/actions module
type Action = ();

#[derive(Default, Debug, Clone)]
pub struct SharedJsContext {
    pub(crate) task_results: Arc<TaskResults>,
    pub(crate) action_map: Arc<DashMap<TargetId, Vec<Action>>>,
    pub(crate) env_map: Arc<DashMap<TargetId, FxHashMap<String, String>>>,
    pub(crate) loaded_modules: FxHashMap<String, ()>,
    pub(crate) loaded_rules: FxHashMap<String, Rule>,
    pub(crate) output_map: Arc<DashMap<TargetId, Vec<PathBuf>>>,
    pub(crate) provides_map: Arc<DashMap<TargetId, FxHashMap<String, String>>>,
    pub(crate) rule_map: Arc<DashMap<String, Rule>>,
    pub(crate) run_script_map: Arc<DashMap<TargetId, RunScript>>,
}

#[derive(Default, Clone, Debug)]
pub struct FfiContext {
    pub(crate) id: Uuid,
    pub(crate) action_map: Arc<DashMap<TargetId, Vec<Action>>>,
    pub(crate) env_map: Arc<DashMap<TargetId, FxHashMap<String, String>>>,
    pub(crate) output_map: Arc<DashMap<TargetId, Vec<PathBuf>>>,
    pub(crate) provides_map: Arc<DashMap<TargetId, FxHashMap<String, String>>>,
    pub(crate) rule_map: Arc<DashMap<String, Rule>>,
    pub(crate) run_script_map: Arc<DashMap<TargetId, RunScript>>,
}

impl From<SharedJsContext> for FfiContext {
    fn from(ctx: SharedJsContext) -> Self {
        Self {
            id: Uuid::new_v4(),
            rule_map: ctx.rule_map,
            run_script_map: ctx.run_script_map,
            action_map: ctx.action_map,
            output_map: ctx.output_map,
            provides_map: ctx.provides_map,
            env_map: ctx.env_map,
        }
    }
}
