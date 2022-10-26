use super::*;
use fxhash::FxHashSet;
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
    RuleExecutorError(RuleExecutorError),

    #[error(transparent)]
    RuleStoreError(RuleStoreError),
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
                .map_err(TargetPlannerError::RuleExecutorError)?,
            label_registry,
        })
    }

    #[tracing::instrument(name = "TargetPlanner::update", skip(self, target))]
    pub async fn update(&mut self, target: &ExecutableTarget) -> Result<(), TargetPlannerError> {
        self.rule_executor
            .update_provide_map(target, &self.store)
            .await
            .map_err(TargetPlannerError::RuleExecutorError)
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
            .map_err(TargetPlannerError::RuleStoreError)?;

        let rule = self
            .rule_executor
            .load_rule(&rule_name, rule_file)
            .await
            .map_err(TargetPlannerError::RuleExecutorError)?;

        let toolchains = self.find_toolchains(target, &rule)?;

        let deps = self.find_deps(target)?;

        let transitive_deps = self.find_transitive_deps(target)?;

        let exec_result = self
            .rule_executor
            .execute(
                target_plan_started_at,
                env,
                &rule,
                target,
                &deps,
                &transitive_deps,
            )
            .await
            .map_err(TargetPlannerError::RuleExecutorError)?;

        ExecutableTarget::new(
            env,
            &rule,
            target,
            &deps,
            &transitive_deps,
            &toolchains,
            exec_result,
        )
        .await
        .map_err(TargetPlannerError::ExecutableTargetError)
    }

    #[tracing::instrument(name = "TargetPlanner::find_toolchains", skip(self))]
    pub fn find_toolchains(
        &self,
        target: &Target,
        rule: &Rule,
    ) -> Result<Vec<TargetManifest>, TargetPlannerError> {
        self._manifests(&target.label, &rule.toolchains, true)
    }

    #[tracing::instrument(name = "TargetPlanner::find_deps", skip(self))]
    pub fn find_deps(&self, target: &Target) -> Result<Vec<TargetManifest>, TargetPlannerError> {
        self._manifests(&target.label, &target.deps, false)
    }

    #[tracing::instrument(name = "TargetPlanner::find_deps", skip(self))]
    pub fn find_runtime_deps(
        &self,
        target: &Target,
    ) -> Result<Vec<TargetManifest>, TargetPlannerError> {
        self._manifests(&target.label, &target.runtime_deps, true)
            .map_err(|err| match err {
                TargetPlannerError::MissingDependencies { deps, label } => {
                    TargetPlannerError::MissingRuntimeDependencies {
                        runtime_deps: deps,
                        label,
                    }
                }
                _ => err,
            })
    }

    #[tracing::instrument(name = "TargetPlanner::find_transitive_deps", skip(self))]
    pub fn find_transitive_deps(
        &self,
        target: &Target,
    ) -> Result<Vec<TargetManifest>, TargetPlannerError> {
        self._manifests(&target.label, &target.deps, true)
    }

    pub fn _manifests(
        &self,
        label: &Label,
        deps: &[Label],
        transitive: bool,
    ) -> Result<Vec<TargetManifest>, TargetPlannerError> {
        let mut manifests = FxHashSet::default();

        let label = self.label_registry.register(label.clone());
        let deps = self.label_registry.register_many(deps);

        for label in self._deps(label, &deps, transitive)? {
            manifests.insert(self.build_results.get_manifest(label).unwrap());
        }

        Ok(manifests.into_iter().collect())
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
            if self.build_results.has_manifest(*dep) {
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
