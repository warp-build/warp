use super::*;
use crate::sync::Arc;
use thiserror::*;

pub struct TargetPlanner {
    build_results: Arc<BuildResults>,
    rule_store: Arc<RuleStore>,
    rule_executor: RuleExecutor,
    source_manager: Arc<SourceManager>,
    artifact_store: Arc<ArtifactStore>,
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
        skip(build_results, artifact_store, share_rule_executor_state)
    )]
    pub fn new(
        build_results: Arc<BuildResults>,
        artifact_store: Arc<ArtifactStore>,
        source_manager: Arc<SourceManager>,
        share_rule_executor_state: Arc<SharedRuleExecutorState>,
        label_registry: Arc<LabelRegistry>,
    ) -> Result<Self, TargetPlannerError> {
        Ok(Self {
            build_results,
            artifact_store,
            source_manager,
            rule_store: share_rule_executor_state.rule_store.clone(),
            rule_executor: RuleExecutor::new(share_rule_executor_state)
                .map_err(|err| TargetPlannerError::RuleExecutorError(Box::new(err)))?,
            label_registry,
        })
    }

    #[tracing::instrument(name = "TargetPlanner::update", skip(self, target))]
    pub async fn update(&mut self, target: &ExecutableTarget) -> Result<(), TargetPlannerError> {
        self.rule_executor
            .update_provide_map(target, &self.artifact_store)
            .await
            .map_err(|err| TargetPlannerError::RuleExecutorError(Box::new(err)))
    }

    #[tracing::instrument(name = "TargetPlanner::plan", skip(self, env))]
    pub async fn plan(
        &mut self,
        build_opts: &BuildOpts,
        env: &ExecutionEnvironment,
        label_id: LabelId,
        goal: Goal,
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

        let toolchains = self.find_toolchains(label_id, target, &rule)?;

        let deps = self.find_deps(label_id, target)?;

        let transitive_deps = self.find_transitive_deps(label_id, target)?;

        let runtime_deps = if rule.kind.is_runnable() {
            self.find_runtime_deps(label_id, target)?
        } else {
            target
                .runtime_deps
                .iter()
                .map(|d| self.label_registry.register_label(d.to_owned()))
                .collect()
        };

        let mut exec_result = self
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

        if exec_result.srcs.len() == 1 {
            let label = self.label_registry.get_label(label_id);
            let symbol = SourceSymbol::from_label_and_goal(&label, goal);

            if let Ok(source_chunk) = self
                .source_manager
                .get_source_chunk_by_symbol(label_id, &label, &symbol)
                .await
            {
                exec_result.srcs = [SourceInput::Chunk(source_chunk)].into_iter().collect();
            }
        }

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
            &self.label_registry,
        )
        .await
        .map_err(TargetPlannerError::ExecutableTargetError)
    }

    #[tracing::instrument(name = "TargetPlanner::find_toolchains", skip(self))]
    pub fn find_toolchains(
        &self,
        label: LabelId,
        target: &Target,
        rule: &Rule,
    ) -> Result<Vec<LabelId>, TargetPlannerError> {
        self._manifests(label, &rule.toolchains, true, false)
    }

    #[tracing::instrument(name = "TargetPlanner::find_deps", skip(self))]
    pub fn find_deps(
        &self,
        label: LabelId,
        target: &Target,
    ) -> Result<Vec<LabelId>, TargetPlannerError> {
        self._manifests(label, &target.deps, false, false)
    }

    #[tracing::instrument(name = "TargetPlanner::find_deps", skip(self))]
    pub fn find_runtime_deps(
        &self,
        label: LabelId,
        target: &Target,
    ) -> Result<Vec<LabelId>, TargetPlannerError> {
        self._manifests(label, &target.runtime_deps, true, true)
    }

    #[tracing::instrument(name = "TargetPlanner::find_transitive_deps", skip(self))]
    pub fn find_transitive_deps(
        &self,
        label: LabelId,
        target: &Target,
    ) -> Result<Vec<LabelId>, TargetPlannerError> {
        self._manifests(label, &target.deps, true, false)
    }

    pub fn _manifests(
        &self,
        label: LabelId,
        deps: &[Label],
        transitive: bool,
        runtime: bool,
    ) -> Result<Vec<LabelId>, TargetPlannerError> {
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
        let mut collected_deps: Vec<LabelId> = vec![];
        let mut missing_deps: Vec<LabelId> = vec![];

        let mut pending: Vec<LabelId> = deps.to_vec();
        let mut visited: Vec<LabelId> = vec![];

        while let Some(dep) = pending.pop() {
            if visited.contains(&dep) {
                continue;
            }
            visited.push(dep);

            if self.build_results.is_label_built(dep) {
                collected_deps.push(dep);
                let node_deps = self.build_results.get_target_runtime_deps(dep);
                pending.extend(node_deps);
            } else {
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
