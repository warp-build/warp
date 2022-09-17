use super::*;
use fxhash::FxHashSet;
use std::sync::Arc;
use thiserror::*;

pub struct TargetPlanner {
    build_results: Arc<BuildResults>,
    rule_store: RuleStore,
    rule_executor: RuleExecutor,
    store: Arc<Store>,
}

#[derive(Error, Debug)]
pub enum TargetPlannerError {
    #[error("When planning {label:?}, the following dependencies were missing: {deps:?}")]
    MissingDependencies { deps: Vec<Label>, label: Label },

    #[error(transparent)]
    ExecutableTargetError(ExecutableTargetError),

    #[error(transparent)]
    RuleExecutorError(RuleExecutorError),

    #[error(transparent)]
    RuleStoreError(RuleStoreError),
}

impl TargetPlanner {
    #[tracing::instrument(name = "TargetPlanner::new", skip(workspace, build_results))]
    pub fn new(
        workspace: &Workspace,
        build_results: Arc<BuildResults>,
        store: Arc<Store>,
        share_rule_executor_state: Arc<SharedRuleExecutorState>,
    ) -> Result<Self, TargetPlannerError> {
        Ok(Self {
            build_results,
            store,
            rule_store: RuleStore::new(workspace),
            rule_executor: RuleExecutor::new(share_rule_executor_state)
                .map_err(TargetPlannerError::RuleExecutorError)?,
        })
    }

    #[tracing::instrument(name = "TargetPlanner::update", skip(self))]
    pub async fn update(&mut self, target: &ExecutableTarget) -> Result<(), TargetPlannerError> {
        self.rule_executor
            .update_provide_map(target, &self.store)
            .await
            .map_err(TargetPlannerError::RuleExecutorError)
    }

    #[tracing::instrument(name = "TargetPlanner::plan", skip(self, env))]
    pub async fn plan(
        &mut self,
        env: &ExecutionEnvironment,
        target: &Target,
    ) -> Result<ExecutableTarget, TargetPlannerError> {
        let (rule_file, rule_name) = self
            .rule_store
            .get(&target.rule_name)
            .await
            .map_err(TargetPlannerError::RuleStoreError)?;

        let rule = self
            .rule_executor
            .load_rule(&rule_name, rule_file)
            .await
            .map_err(TargetPlannerError::RuleExecutorError)?;

        self.find_toolchains(target, &rule)?;

        let deps = self.find_deps(target)?;

        let transitive_deps = self.find_transitive_deps(target)?;

        let exec_result = self
            .rule_executor
            .execute(env, &rule, target, &deps, &transitive_deps)
            .await
            .map_err(TargetPlannerError::RuleExecutorError)?;

        ExecutableTarget::new(env, &rule, target, &deps, &transitive_deps, exec_result)
            .await
            .map_err(TargetPlannerError::ExecutableTargetError)
    }

    #[tracing::instrument(name = "TargetPlanner::find_toolchains", skip(self))]
    pub fn find_toolchains(&self, target: &Target, rule: &Rule) -> Result<(), TargetPlannerError> {
        self._deps(&target.label, &rule.toolchains, true)
            .map(|_| ())
    }

    #[tracing::instrument(name = "TargetPlanner::find_deps", skip(self))]
    pub fn find_deps(&self, target: &Target) -> Result<Vec<Dependency>, TargetPlannerError> {
        self._deps(&target.label, &target.deps, false)
    }

    #[tracing::instrument(name = "TargetPlanner::find_transitive_deps", skip(self))]
    pub fn find_transitive_deps(
        &self,
        target: &Target,
    ) -> Result<Vec<Dependency>, TargetPlannerError> {
        self._deps(&target.label, &target.deps, true)
    }

    pub fn _deps(
        &self,
        label: &Label,
        deps: &[Label],
        transitive: bool,
    ) -> Result<Vec<Dependency>, TargetPlannerError> {
        let mut collected_deps: FxHashSet<Dependency> = FxHashSet::default();
        let mut missing_deps: FxHashSet<Label> = FxHashSet::default();

        for dep in deps {
            if let Some((_manifest, node)) = self.build_results.get_computed_target(dep) {
                collected_deps.insert(node.to_dependency());

                if transitive {
                    let node_deps = node
                        .deps
                        .iter()
                        .map(|d| d.label.clone())
                        .collect::<Vec<Label>>();

                    let node_deps = node_deps.as_slice();

                    for dep in self._deps(&node.label, node_deps, transitive)? {
                        collected_deps.insert(dep);
                    }
                }
            } else {
                missing_deps.insert(dep.clone());
            }
        }

        if !missing_deps.is_empty() {
            // NOTE(@ostera): we want to build the things that are furthest away from our target
            // first, so the dependencies of our dependencies.
            Err(TargetPlannerError::MissingDependencies {
                label: label.clone(),
                deps: missing_deps.iter().cloned().collect::<Vec<Label>>(),
            })
        } else {
            let deps = collected_deps.iter().cloned().collect::<Vec<Dependency>>();
            Ok(deps)
        }
    }
}

mod tests {}
