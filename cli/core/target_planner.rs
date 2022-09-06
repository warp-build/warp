pub struct TargetPlanner {
    build_results: Arc<BuildResults>,
    rule_loader: Arc<RuleLoader>,
    rule_executor: Arc<RuleExecutor>,
}

impl TargetPlanner {
    pub fn plan(&self, target: &Target) -> Result<ExecutableTarget, TargetPlannerError> {
        let transitive_deps = self.find_transitive_deps(&target).await?;

        let rule = self.rule_loader.load(&target.rule).await?;

        let exec_result = self.rule_executor.execute(env, rule, target).await?;

        Ok(ExecutableTarget {
            actions: exec_result.action,
            // ...
        })
    }

    pub fn find_transitive_deps(
        &self,
        target: &Target,
    ) -> Result<Vec<Dependency>, TargetPlannerError> {
        let mut deps: FxHashSet<Dependency> = FxHashSet::default();
        let mut missing_deps: FxHashSet<Label> = FxHashSet::default();

        if let Some(this_deps) = &target.deps {
            for dep in this_deps {
                if let Some(mut node) = find_node(dep.label.clone()) {
                    deps.insert(dep.clone());
                    for dep in node.transitive_deps(find_node)? {
                        deps.insert(dep);
                    }
                } else {
                    missing_deps.insert(dep.label.clone());
                }
            }
        }

        if !missing_deps.is_empty() {
            let mut missing_deps = missing_deps.iter().cloned().collect::<Vec<Label>>();
            missing_deps.sort();
            Err(ComputedTargetError::MissingDependencies {
                label: target.target.label().clone(),
                deps: missing_deps,
            })
        } else {
            let transitive_deps = deps;
            let mut deps = transitive_deps.iter().cloned().collect::<Vec<Dependency>>();
            deps.sort();
            target.transitive_deps = Some(transitive_deps);
            Ok(deps.to_vec())
        }
    }
}

mod tests {}
