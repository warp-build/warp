use std::ops::ControlFlow;
use std::pin::Pin;

use super::{DefaultPlannerContext, Dependencies, Planner, PlannerError, PlanningFlow};
use crate::model::{ExecutableSpec, Signature, TargetId};
use crate::rules::RuleExecutor;
use crate::worker::ExecutionEnvironment;
use futures::{Future, FutureExt};

pub struct DefaultPlanner<RE: RuleExecutor> {
    ctx: DefaultPlannerContext,
    rule_executor: RE,
}

impl<RE, Ctx> Planner for DefaultPlanner<RE>
where
    RE: RuleExecutor<Context = Ctx>,
    Ctx: From<DefaultPlannerContext>,
{
    type Context = DefaultPlannerContext;

    fn new(ctx: Self::Context) -> Result<Self, PlannerError> {
        Ok(Self {
            rule_executor: RE::new(ctx.clone().into())?,
            ctx,
        })
    }

    fn plan<'a>(
        &'a mut self,
        sig: Signature,
        env: ExecutionEnvironment,
    ) -> Pin<Box<dyn Future<Output = Result<PlanningFlow, PlannerError>> + 'a>> {
        async move {
            let planning_start_time = chrono::Utc::now();

            let deps = match self.find_deps(&sig).await? {
                PlanningFlow::FoundAllDeps { deps } => deps,
                flow => return Ok(flow),
            };

            let plan = self.rule_executor.execute(&env, &sig, &deps).await?;

            let planning_end_time = chrono::Utc::now();

            let spec = ExecutableSpec::builder()
                .target(sig.target().clone())
                .planning_start_time(planning_start_time)
                .planning_end_time(planning_end_time)
                .deps(deps)
                .build()?;

            Ok(PlanningFlow::Planned { spec })
        }
        .boxed_local()
    }
}

impl<RE: RuleExecutor> DefaultPlanner<RE> {
    async fn find_deps(&self, sig: &Signature) -> Result<PlanningFlow, PlannerError> {
        let deps = self.ctx.target_registry.register_many_targets(sig.deps());

        let compile_deps = match self._deps(sig.target().target_id(), &deps) {
            DepFinderFlow::MissingDeps(deps) => return Ok(PlanningFlow::MissingDeps { deps }),
            DepFinderFlow::AllDepsFound(deps) => deps,
        };

        let deps = Dependencies::builder()
            .compile_deps(compile_deps)
            .toolchains(vec![])
            .transitive_deps(vec![])
            .runtime_deps(vec![])
            .build()?;

        Ok(PlanningFlow::FoundAllDeps { deps })
    }

    fn _deps(&self, target: TargetId, deps: &[TargetId]) -> DepFinderFlow {
        let mut collected_deps: Vec<TargetId> = vec![];
        let mut missing_deps: Vec<TargetId> = vec![];

        let mut pending: Vec<TargetId> = deps.to_vec();
        let mut visited: Vec<TargetId> = vec![target];

        while let Some(dep) = pending.pop() {
            if visited.contains(&dep) {
                continue;
            }
            visited.push(dep);

            if self.ctx.task_results.is_target_built(dep) {
                collected_deps.push(dep);
                if let Some(task_result) = self.ctx.task_results.get_task_result(dep) {
                    let deps = task_result.executable_spec.deps();
                    pending.extend(deps.compile_deps());
                }
            } else {
                missing_deps.push(dep);
            }
        }

        if !missing_deps.is_empty() {
            DepFinderFlow::MissingDeps(missing_deps)
        } else {
            DepFinderFlow::AllDepsFound(collected_deps)
        }
    }
}

enum DepFinderFlow {
    AllDepsFound(Vec<TargetId>),
    MissingDeps(Vec<TargetId>),
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::archive::ArchiveManager;
    use crate::model::ConcreteTarget;
    use crate::resolver::TargetRegistry;
    use crate::rules::RuleExecutorError;
    use crate::store::DefaultStore;
    use crate::sync::Arc;
    use crate::worker::TaskResults;
    use crate::{Config, Goal};

    struct NoopRuleExecutor;
    impl RuleExecutor for NoopRuleExecutor {
        type Context = ();

        fn new(_ctx: Self::Context) -> Result<Self, RuleExecutorError> {
            Ok(Self)
        }

        fn execute<'a>(
            &'a mut self,
            _env: &ExecutionEnvironment,
            _sig: &Signature,
            _deps: &Dependencies,
        ) -> Pin<Box<dyn Future<Output = Result<(), RuleExecutorError>> + 'a>> {
            async move { Ok(()) }.boxed_local()
        }
    }

    #[tokio::test]
    async fn plans_a_signature_for_execution() {
        let config = Config::builder().build().unwrap();
        let archive_manager = ArchiveManager::new(&config).into();
        let store = DefaultStore::new(config, archive_manager).into();
        let target_registry = Arc::new(TargetRegistry::new());
        let task_results = TaskResults::new(target_registry.clone()).into();
        let ctx = DefaultPlannerContext::new(store, target_registry, task_results);

        let goal = Goal::Build;
        let target = Arc::new("./my/file.ex".into());
        let target_id = ctx.target_registry.register_target(&target);
        let target = ConcreteTarget::new(goal, target_id, target, "".into());

        let sig = Signature::builder()
            .rule("test_rule".into())
            .target(target)
            .build()
            .unwrap();

        let env = ExecutionEnvironment::default();

        let mut p: DefaultPlanner<NoopRuleExecutor> = DefaultPlanner::new(ctx).unwrap();
        let flow = p.plan(sig.clone(), env).await.unwrap();

        assert_matches!(flow, PlanningFlow::Planned { spec } if spec.target() == sig.target());
    }

    #[tokio::test]
    async fn when_missing_dependencies_we_abort() {
        let config = Config::builder().build().unwrap();
        let archive_manager = ArchiveManager::new(&config).into();
        let store = DefaultStore::new(config, archive_manager).into();
        let target_registry = Arc::new(TargetRegistry::new());
        let task_results = TaskResults::new(target_registry.clone()).into();
        let ctx = DefaultPlannerContext::new(store, target_registry, task_results);

        let goal = Goal::Build;
        let target = Arc::new("./my/file.ex".into());
        let target_id = ctx.target_registry.register_target(&target);
        let target = ConcreteTarget::new(goal, target_id, target, "".into());

        let sig = Signature::builder()
            .rule("test_rule".into())
            .target(target)
            .deps(vec!["./my/dep.ex".into()])
            .build()
            .unwrap();

        let env = ExecutionEnvironment::default();

        let mut p: DefaultPlanner<NoopRuleExecutor> = DefaultPlanner::new(ctx).unwrap();
        let flow = p.plan(sig.clone(), env).await.unwrap();

        assert_matches!(flow, PlanningFlow::MissingDeps { deps } => {
            dbg!(&deps);
            assert!(!deps.is_empty());
        });
    }
}
