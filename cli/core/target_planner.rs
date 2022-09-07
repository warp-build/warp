use super::*;
use fxhash::FxHashSet;
use std::sync::Arc;
use thiserror::*;

pub struct TargetPlanner {
    build_results: Arc<BuildResults>,
    rule_loader: RuleLoader,
    rule_executor: RuleExecutor,
}

#[derive(Error, Debug)]
pub enum TargetPlannerError {
    #[error("When planning {label:?}, the following dependencies were missing: {deps:?}")]
    MissingDependencies { deps: Vec<Dependency>, label: Label },

    #[error(transparent)]
    ExecutableTargetError(ExecutableTargetError),
}

impl TargetPlanner {
    pub fn new(build_results: Arc<BuildResults>) -> Self {
        Self {
            build_results,
            rule_loader,
            rule_executor: RuleExecutor::new(),
        }
    }

    pub async fn plan(&self, target: &Target) -> Result<ExecutableTarget, TargetPlannerError> {
        let transitive_deps = self.find_transitive_deps(&target).await?;

        let rule = self.rule_loader.load(&target.rule).await?;

        let exec_result = self
            .rule_executor
            .execute(&env, &rule, &target, &transitive_deps)
            .await?;

        ExecutableTarget::new(&env, &rule, &target, &transitive_deps, &exec_result)
            .map_err(TargetPlannerError::ExecutableTargetError)
    }

    pub async fn find_transitive_deps(
        &self,
        target: &Target,
    ) -> Result<Vec<Dependency>, TargetPlannerError> {
        let mut deps: FxHashSet<Dependency> = FxHashSet::default();
        let mut missing_deps: FxHashSet<Label> = FxHashSet::default();

        if let Some(this_deps) = &target.deps {
            for dep in this_deps {
                if let Some(mut node) = self.build_results.get_computed_target(dep.label.clone()) {
                    deps.insert(dep.clone());
                    for dep in self.find_transitive_deps(node)? {
                        deps.insert(dep);
                    }
                } else {
                    missing_deps.insert(dep.label.clone());
                }
            }
        }

        if !missing_deps.is_empty() {
            // NOTE(@ostera): we want to build the things that are furthest away from our target
            // first, so the dependencies of our dependencies.
            let mut missing_deps = missing_deps.iter().rev().cloned().collect::<Vec<Label>>();
            Err(TargetPlannerError::MissingDependencies {
                label: target.label.clone(),
                deps: missing_deps,
            })
        } else {
            let mut deps = transitive_deps.iter().cloned().collect::<Vec<Dependency>>();
            Ok(deps.to_vec())
        }
    }
}

mod tests {}
