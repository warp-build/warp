use super::*;
use std::sync::Arc;
use thiserror::*;

pub struct TargetPlanner {
    build_results: Arc<BuildResults>,
    rule_store: Arc<RuleStore>,
    rule_executor: RuleExecutor,
    store: Arc<Store>,
    label_registry: Arc<LabelRegistry>,
}

#[derive(Error, Debug)]
pub enum TargetPlannerError {
    #[error("When planning {label:?}, the following dependencies were missing: {deps:?}")]
    MissingDependencies { deps: Vec<LabelId>, label: LabelId },

    #[error("When planning {label:?}, the following dependencies were missing: {runtime_deps:?}")]
    MissingRuntimeDependencies {
        runtime_deps: Vec<LabelId>,
        label: LabelId,
    },

    #[error(transparent)]
    ExecutableTargetError(ExecutableTargetError),

    #[error(transparent)]
    RuleExecutorError(Box<RuleExecutorError>),

    #[error(transparent)]
    RuleStoreError(Box<RuleStoreError>),
}

impl TargetPlanner {
    #[tracing::instrument(
        name = "TargetPlanner::new",
        skip(build_results, store, share_rule_executor_state)
    )]
    pub fn new(
        build_results: Arc<BuildResults>,
        store: Arc<Store>,
        share_rule_executor_state: Arc<SharedRuleExecutorState>,
        label_registry: Arc<LabelRegistry>,
    ) -> Result<Self, TargetPlannerError> {
        Ok(Self {
            build_results,
            store,
            rule_store: share_rule_executor_state.rule_store.clone(),
            rule_executor: RuleExecutor::new(share_rule_executor_state)
                .map_err(|err| TargetPlannerError::RuleExecutorError(Box::new(err)))?,
            label_registry,
        })
    }

    #[tracing::instrument(name = "TargetPlanner::update", skip(self, target))]
    pub async fn update(&mut self, target: &ExecutableTarget) -> Result<(), TargetPlannerError> {
        self.rule_executor
            .update_provide_map(target, &self.store)
            .await
            .map_err(|err| TargetPlannerError::RuleExecutorError(Box::new(err)))
    }

    #[tracing::instrument(name = "TargetPlanner::plan", skip(self, env))]
    pub async fn plan(
        &mut self,
        build_opts: &BuildOpts,
        env: &ExecutionEnvironment,
        target: &Target,
    ) -> Result<ExecutableTarget, TargetPlannerError> {
        let target_plan_started_at = chrono::Utc::now();

        let (rule_file, rule_name) = self
            .rule_store
            .get(&target.rule_name)
            .await
            .map_err(|err| TargetPlannerError::RuleStoreError(Box::new(err)))?;

        let rule = self
            .rule_executor
            .load_rule(&rule_name, rule_file)
            .await
            .map_err(|err| TargetPlannerError::RuleExecutorError(Box::new(err)))?;

        let toolchains = self.find_toolchains(target, &rule)?;

        let deps = self.find_deps(target)?;

        let transitive_deps = self.find_transitive_deps(target)?;

        let runtime_deps = if rule.kind.is_runnable() {
            self.find_runtime_deps(target)?
        } else {
            target
                .runtime_deps
                .iter()
                .map(|d| self.label_registry.register_label(d))
                .collect()
        };

        let exec_result = self
            .rule_executor
            .execute(
                target_plan_started_at,
                env,
                &rule,
                target,
                &deps,
                &transitive_deps,
                &runtime_deps,
            )
            .await
            .map_err(|err| TargetPlannerError::RuleExecutorError(Box::new(err)))?;

        ExecutableTarget::new(
            env,
            &rule,
            target,
            &deps,
            &runtime_deps,
            &transitive_deps,
            &toolchains,
            exec_result,
            &self.build_results,
        )
        .await
        .map_err(TargetPlannerError::ExecutableTargetError)
    }

    #[tracing::instrument(name = "TargetPlanner::find_toolchains", skip(self))]
    pub fn find_toolchains(
        &self,
        target: &Target,
        rule: &Rule,
    ) -> Result<Vec<LabelId>, TargetPlannerError> {
        self._manifests(&target.label, &rule.toolchains, true, false)
    }

    #[tracing::instrument(name = "TargetPlanner::find_deps", skip(self))]
    pub fn find_deps(&self, target: &Target) -> Result<Vec<LabelId>, TargetPlannerError> {
        self._manifests(&target.label, &target.deps, false, false)
    }

    #[tracing::instrument(name = "TargetPlanner::find_deps", skip(self))]
    pub fn find_runtime_deps(&self, target: &Target) -> Result<Vec<LabelId>, TargetPlannerError> {
        self._manifests(&target.label, &target.runtime_deps, true, true)
    }

    #[tracing::instrument(name = "TargetPlanner::find_transitive_deps", skip(self))]
    pub fn find_transitive_deps(
        &self,
        target: &Target,
    ) -> Result<Vec<LabelId>, TargetPlannerError> {
        self._manifests(&target.label, &target.deps, true, false)
    }

    pub fn _manifests(
        &self,
        label: &Label,
        deps: &[Label],
        transitive: bool,
        runtime: bool,
    ) -> Result<Vec<LabelId>, TargetPlannerError> {
        let label = self.label_registry.register_label(label);
        let deps = self.label_registry.register_many_labels(deps);

        let labels = if runtime {
            self._runtime_deps(label, &deps)?
        } else {
            self._deps(label, &deps, transitive)?
        };

        Ok(labels)
    }

    pub fn _runtime_deps(
        &self,
        label: LabelId,
        deps: &[LabelId],
    ) -> Result<Vec<LabelId>, TargetPlannerError> {
        let mut collected_labels: Vec<Arc<Label>> = vec![];
        let mut missing_labels: Vec<Arc<Label>> = vec![];

        let mut collected_deps: Vec<LabelId> = vec![];
        let mut missing_deps: Vec<LabelId> = vec![];

        let mut pending: Vec<LabelId> = deps.to_vec();
        let mut visited: Vec<LabelId> = vec![];

        while let Some(dep) = pending.pop() {
            if visited.contains(&dep) {
                continue;
            }
            visited.push(dep);

            let dep_label = self.label_registry.get_label(dep).to_owned();
            if self.build_results.is_label_built(dep) {
                collected_deps.push(dep);
                collected_labels.push(dep_label);
                let node_deps = self.build_results.get_target_runtime_deps(dep);
                pending.extend(node_deps);
            } else {
                missing_labels.push(dep_label);
                missing_deps.push(dep);
            }
        }

        if !missing_deps.is_empty() {
            Err(TargetPlannerError::MissingDependencies {
                label,
                deps: missing_deps,
            })
        } else {
            Ok(collected_deps)
        }
    }

    pub fn _deps(
        &self,
        label: LabelId,
        deps: &[LabelId],
        transitive: bool,
    ) -> Result<Vec<LabelId>, TargetPlannerError> {
        let mut collected_deps: Vec<LabelId> = vec![];
        let mut missing_deps: Vec<LabelId> = vec![];

        for dep in deps {
            if self.build_results.is_label_built(*dep) {
                collected_deps.push(*dep);

                if transitive {
                    let node_deps = self.build_results.get_target_deps(*dep);
                    for dep in self._deps(*dep, &node_deps, transitive)? {
                        collected_deps.push(dep);
                    }
                }
            } else {
                missing_deps.push(*dep);
            }
        }

        if !missing_deps.is_empty() {
            // NOTE(@ostera): we want to build the things that are furthest away from our target
            // first, so the dependencies of our dependencies.
            Err(TargetPlannerError::MissingDependencies {
                label,
                deps: missing_deps,
            })
        } else {
            Ok(collected_deps)
        }
    }
}

mod tests {}
