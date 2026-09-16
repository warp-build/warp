use crate::executor::actions::Action;
use crate::model::{Rule, RunScript, TaskId, TestRunner};
use crate::resolver::TargetRegistry;
use crate::rules::store::RuleStore;
use crate::sync::*;
use crate::worker::{TaskRegistry, TaskResults};
use dashmap::DashMap;
use fxhash::FxHashMap;
use std::path::PathBuf;

#[derive(Debug, Clone)]
pub struct SharedJsContext {
    pub(crate) action_map: Arc<DashMap<TaskId, Vec<Action>>>,
    pub(crate) loaded_modules: FxHashMap<String, ()>,
    pub(crate) loaded_rules: FxHashMap<String, Rule>,
    pub(crate) output_map: Arc<DashMap<TaskId, Vec<PathBuf>>>,
    pub(crate) provides_map: Arc<DashMap<TaskId, FxHashMap<String, String>>>,
    pub(crate) rule_map: Arc<DashMap<String, Rule>>,
    pub(crate) rule_store: Arc<RuleStore>,
    pub(crate) run_script_map: Arc<DashMap<TaskId, RunScript>>,
    pub(crate) shell_env_map: Arc<DashMap<TaskId, FxHashMap<String, String>>>,
    pub(crate) target_registry: Arc<TargetRegistry>,
    pub(crate) task_registry: Arc<TaskRegistry>,
    pub(crate) task_results: Arc<TaskResults>,
    pub(crate) test_runner_map: Arc<DashMap<TaskId, TestRunner>>,
}

impl SharedJsContext {
    pub fn new(
        target_registry: Arc<TargetRegistry>,
        task_registry: Arc<TaskRegistry>,
        task_results: Arc<TaskResults>,
        rule_store: Arc<RuleStore>,
    ) -> SharedJsContext {
        Self {
            action_map: Default::default(),
            shell_env_map: Default::default(),
            loaded_modules: Default::default(),
            loaded_rules: Default::default(),
            output_map: Default::default(),
            provides_map: Default::default(),
            rule_map: Default::default(),
            rule_store,
            run_script_map: Default::default(),
            test_runner_map: Default::default(),
            target_registry,
            task_registry,
            task_results,
        }
    }
}

#[derive(Default, Clone, Debug)]
pub struct FfiContext {
    pub(crate) action_map: Arc<DashMap<TaskId, Vec<Action>>>,
    pub(crate) shell_env_map: Arc<DashMap<TaskId, FxHashMap<String, String>>>,
    pub(crate) output_map: Arc<DashMap<TaskId, Vec<PathBuf>>>,
    pub(crate) provides_map: Arc<DashMap<TaskId, FxHashMap<String, String>>>,
    pub(crate) rule_map: Arc<DashMap<String, Rule>>,
    pub(crate) run_script_map: Arc<DashMap<TaskId, RunScript>>,
    pub(crate) test_runner_map: Arc<DashMap<TaskId, TestRunner>>,
    pub(crate) target_registry: Arc<TargetRegistry>,
    pub(crate) task_registry: Arc<TaskRegistry>,
}

impl From<SharedJsContext> for FfiContext {
    fn from(ctx: SharedJsContext) -> Self {
        Self {
            action_map: ctx.action_map,
            shell_env_map: ctx.shell_env_map,
            output_map: ctx.output_map,
            provides_map: ctx.provides_map,
            rule_map: ctx.rule_map,
            run_script_map: ctx.run_script_map,
            test_runner_map: ctx.test_runner_map,
            target_registry: ctx.target_registry,
            task_registry: ctx.task_registry,
        }
    }
}
