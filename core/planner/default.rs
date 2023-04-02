use super::{DefaultPlannerContext, Dependencies, Planner, PlannerError, PlanningFlow};

use crate::model::{
    ExecutableSpec, ExecutionEnvironment, RemoteTarget, Requirement, Signature, SourceKind, Task,
    UnregisteredTask,
};
use crate::rules::RuleExecutor;

use crate::tricorder::SignatureGenerationFlow;
use crate::{Goal, Target};
use async_trait::async_trait;
use fxhash::FxHashSet;
use tracing::instrument;

pub struct DefaultPlanner<RE: RuleExecutor> {
    ctx: DefaultPlannerContext,
    rule_executor: RE,
}

#[async_trait(?Send)]
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

    #[instrument(name = "DefaultPlanner::plan", skip(self, env))]
    async fn plan(
        &mut self,
        task: Task,
        sig: &Signature,
        env: ExecutionEnvironment,
    ) -> Result<PlanningFlow, PlannerError> {
        let planning_start_time = chrono::Utc::now();

        let compile_deps = match self._deps(task, sig.deps(), DepKind::Compile) {
            DepFinderFlow::MissingDeps(deps) => return Ok(PlanningFlow::MissingDeps { deps }),
            DepFinderFlow::AllDepsFound(deps) => deps, //vec![deps, prior_deps].concat(),
        };

        let transitive_compile_deps =
            match self._deps(task, &compile_deps, DepKind::TransitiveCompile) {
                DepFinderFlow::MissingDeps(deps) => return Ok(PlanningFlow::MissingDeps { deps }),
                DepFinderFlow::AllDepsFound(deps) => deps,
            };

        // NB(@ostera): runtime_deps and transitive_runtime_deps are *not needed* for
        // planning/executing. They just need to be built before you can execute the _results_ of
        // whatever you're running. Otherwise we can have dependency cycles.
        let runtime_deps = match self._deps(task, sig.runtime_deps(), DepKind::Runtime) {
            DepFinderFlow::MissingDeps(deps) if task.goal().is_runnable() => {
                return Ok(PlanningFlow::MissingDeps { deps })
            }
            DepFinderFlow::MissingDeps(deps) | DepFinderFlow::AllDepsFound(deps) => deps,
        };

        let transitive_runtime_deps =
            match self._deps(task, &runtime_deps, DepKind::TransitiveRuntime) {
                DepFinderFlow::MissingDeps(deps) if task.goal().is_runnable() => {
                    return Ok(PlanningFlow::MissingDeps { deps })
                }
                DepFinderFlow::MissingDeps(deps) | DepFinderFlow::AllDepsFound(deps) => deps,
            };

        let mut deps = Dependencies::builder()
            .toolchains(vec![])
            .compile_deps(compile_deps)
            .runtime_deps(runtime_deps)
            .transitive_compile_deps(transitive_compile_deps)
            .transitive_runtime_deps(transitive_runtime_deps)
            .build()?;

        let plan = self.rule_executor.execute(&env, sig, &deps).await?;

        let toolchains: Vec<Task> = self
            .ctx
            .target_registry
            .register_many_targets(&plan.toolchains)
            .into_iter()
            .map(|id| {
                UnregisteredTask::builder()
                    .goal(Goal::Build)
                    .target_id(id)
                    .build()
                    .unwrap()
            })
            .map(|unreg_task| self.ctx.task_registry.register(unreg_task))
            .collect();

        match self._deps(task, &toolchains, DepKind::Toolchain) {
            DepFinderFlow::MissingDeps(deps) => return Ok(PlanningFlow::MissingDeps { deps }),
            DepFinderFlow::AllDepsFound(toolchains) => deps.set_toolchains(toolchains),
        };

        let planning_end_time = chrono::Utc::now();

        let srcs: FxHashSet<SourceKind> = if task.goal().is_test() {
            let mut srcs = FxHashSet::default();

            for src in plan.srcs.into_iter() {
                let chunk = match self
                    .ctx
                    .code_manager
                    .get_source_chunk(task, sig, &src)
                    .await?
                {
                    SignatureGenerationFlow::ChunkedSource(chunk) => chunk,
                    SignatureGenerationFlow::MissingRequirements { requirements } => {
                        let mut deps = vec![];
                        for req in requirements {
                            let target: Target = match req {
                                Requirement::File { path } => path.into(),
                                Requirement::Url {
                                    url,
                                    tricorder_url,
                                    subpath,
                                } => {
                                    let remote_target = RemoteTarget::builder()
                                        .url(url)
                                        .tricorder_url(tricorder_url)
                                        .subpath(subpath.unwrap())
                                        .build()
                                        .unwrap();
                                    Target::Remote(remote_target)
                                }
                                Requirement::Symbol { .. } => unimplemented!(),
                                Requirement::Dependency { url, .. } => url.into(),
                            };
                            let target_id = self.ctx.target_registry.register_target(target);
                            let unreg_task = UnregisteredTask::builder()
                                .goal(Goal::Build)
                                .target_id(target_id)
                                .build()
                                .unwrap();

                            let task = self.ctx.task_registry.register(unreg_task);

                            deps.push(task)
                        }

                        return Ok(PlanningFlow::MissingDeps { deps });
                    }
                    _ => unreachable!(),
                };

                srcs.insert(chunk);
            }

            srcs
        } else {
            plan.srcs.into_iter().map(SourceKind::File).collect()
        };

        let outs: FxHashSet<SourceKind> = plan.outs.into_iter().map(SourceKind::File).collect();

        let spec = ExecutableSpec::builder()
            .goal(task.goal())
            .target(sig.target().clone())
            .signature(sig.clone())
            .exec_env(env)
            .shell_env(plan.shell_env.into_iter().collect())
            .planning_start_time(planning_start_time)
            .planning_end_time(planning_end_time)
            .srcs(srcs.into())
            .outs(outs.into())
            .provides(plan.provides.into())
            .actions(plan.actions)
            .deps(deps)
            .hash_and_build(&self.ctx.task_results)?;

        // self.ctx.code_manager.save_executable_spec(&sig, &spec).unwrap();

        Ok(PlanningFlow::Planned { spec })
    }
}

impl<RE: RuleExecutor> DefaultPlanner<RE> {
    fn _deps(&self, task: Task, deps: &[Task], kind: DepKind) -> DepFinderFlow {
        let mut collected_deps: Vec<Task> = vec![];
        let mut missing_deps: Vec<Task> = vec![];

        let mut pending: Vec<Task> = deps.to_vec();
        let mut visited: Vec<Task> = vec![task];

        while let Some(dep) = pending.pop() {
            if visited.contains(&dep) {
                continue;
            }
            visited.push(dep);

            if self.ctx.task_results.is_task_completed(dep) {
                collected_deps.push(dep);
                if let Some(task_result) = self.ctx.task_results.get_task_result(&dep) {
                    let deps = task_result.executable_spec.deps();
                    pending.extend(match kind {
                        DepKind::Compile => deps.compile_deps(),
                        DepKind::Runtime => deps.runtime_deps(),
                        DepKind::Toolchain => deps.toolchains(),
                        DepKind::TransitiveCompile => deps.transitive_compile_deps(),
                        DepKind::TransitiveRuntime => deps.transitive_runtime_deps(),
                    });
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

enum DepKind {
    Compile,
    Runtime,
    Toolchain,
    TransitiveCompile,
    TransitiveRuntime,
}

enum DepFinderFlow {
    AllDepsFound(Vec<Task>),
    MissingDeps(Vec<Task>),
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use url::Url;

    use super::*;
    use crate::archive::ArchiveManager;
    use crate::code::CodeManager;
    use crate::model::ConcreteTarget;
    use crate::resolver::{SignatureRegistry, TargetRegistry};
    use crate::rules::{ExecutionResult, RuleExecutorError, RuleStore};
    use crate::store::{ArtifactManifest, DefaultStore, Store, StoreError};
    use crate::sync::Arc;
    use crate::testing::TestMatcherRegistry;
    use crate::worker::{TaskRegistry, TaskResults};
    use crate::{Config, Goal};

    #[derive(Debug, Clone)]
    struct NoopStore;
    #[async_trait]
    impl Store for NoopStore {
        async fn install_from_manifest_url(
            &self,
            _url: &Url,
        ) -> Result<ArtifactManifest, StoreError> {
            Err(StoreError::Unknown)
        }

        async fn find(
            &self,
            _spec: &ExecutableSpec,
        ) -> Result<Option<ArtifactManifest>, StoreError> {
            Err(StoreError::Unknown)
        }

        async fn clean(&self, _spec: &ExecutableSpec) -> Result<(), StoreError> {
            Err(StoreError::Unknown)
        }

        async fn promote(&self, _am: &ArtifactManifest) -> Result<(), StoreError> {
            Err(StoreError::Unknown)
        }

        async fn save(
            &self,
            _spec: &ExecutableSpec,
            _manifest: &ArtifactManifest,
        ) -> Result<(), StoreError> {
            Err(StoreError::Unknown)
        }

        fn get_local_store_path_for_spec(&self, _spec: &ExecutableSpec) -> PathBuf {
            PathBuf::from("")
        }

        fn get_local_store_path_for_manifest(&self, _am: &ArtifactManifest) -> PathBuf {
            PathBuf::from("")
        }

        fn canonicalize_provided_artifact<N: AsRef<str>>(
            &self,
            _am: &ArtifactManifest,
            _name: N,
        ) -> Option<PathBuf> {
            None
        }
    }

    struct NoopRuleExecutor;
    #[async_trait(?Send)]
    impl RuleExecutor for NoopRuleExecutor {
        type Context = ();

        fn new(_ctx: Self::Context) -> Result<Self, RuleExecutorError> {
            Ok(Self)
        }

        async fn execute(
            &mut self,
            _env: &ExecutionEnvironment,
            _sig: &Signature,
            _deps: &Dependencies,
        ) -> Result<ExecutionResult, RuleExecutorError> {
            Ok(Default::default())
        }
    }

    #[tokio::test]
    async fn plans_a_signature_for_execution() {
        let warp_root = assert_fs::TempDir::new().unwrap();
        let config = Config::builder()
            .warp_root(warp_root.path().to_path_buf())
            .build()
            .unwrap();
        let rule_store = RuleStore::new(&config).into();
        let task_registry = Arc::new(TaskRegistry::new());
        let target_registry = Arc::new(TargetRegistry::new());
        let signature_registry = Arc::new(SignatureRegistry::new());
        let test_matcher_registry = Arc::new(TestMatcherRegistry::new());
        let task_results = Arc::new(TaskResults::new(
            task_registry.clone(),
            target_registry.clone(),
            signature_registry.clone(),
        ));
        let archive_manager = Arc::new(ArchiveManager::new(&config));
        let artifact_store = Arc::new(DefaultStore::new(config.clone(), archive_manager));
        let code_manager = Arc::new(
            CodeManager::new(
                config.clone(),
                artifact_store,
                test_matcher_registry.clone(),
                target_registry.clone(),
                task_registry.clone(),
                task_results.clone(),
            )
            .unwrap(),
        );
        let ctx = DefaultPlannerContext::new(
            task_registry.clone(),
            target_registry,
            signature_registry,
            task_results,
            rule_store,
            code_manager,
        );

        let goal = Goal::Build;
        let target = Arc::new("./my/file.ex".into());
        let target_id = ctx.target_registry.register_target(&target);
        let target = ConcreteTarget::new(goal, target_id, target, "".into(), ".".into());

        let sig = Signature::builder()
            .name("test_signature")
            .rule("test_rule")
            .target(target)
            .build()
            .unwrap();

        let env = ExecutionEnvironment::default();

        let unreg_task = UnregisteredTask::builder()
            .goal(goal)
            .target_id(target_id)
            .build()
            .unwrap();
        let task = task_registry.register(unreg_task);

        let mut p: DefaultPlanner<NoopRuleExecutor> = DefaultPlanner::new(ctx).unwrap();
        let flow = p.plan(task, &sig, env).await.unwrap();

        assert_matches!(flow, PlanningFlow::Planned { spec } if spec.target() == sig.target());
    }

    #[tokio::test]
    async fn finds_built_dependencies() {
        let warp_root = assert_fs::TempDir::new().unwrap();
        let config = Config::builder()
            .warp_root(warp_root.path().to_path_buf())
            .build()
            .unwrap();

        let rule_store = RuleStore::new(&config).into();
        let task_registry = Arc::new(TaskRegistry::new());
        let target_registry = Arc::new(TargetRegistry::new());
        let signature_registry = Arc::new(SignatureRegistry::new());
        let test_matcher_registry = Arc::new(TestMatcherRegistry::new());
        let task_results = Arc::new(TaskResults::new(
            task_registry.clone(),
            target_registry.clone(),
            signature_registry.clone(),
        ));
        let archive_manager = Arc::new(ArchiveManager::new(&config));
        let artifact_store = Arc::new(DefaultStore::new(config.clone(), archive_manager));
        let code_manager = Arc::new(
            CodeManager::new(
                config.clone(),
                artifact_store,
                test_matcher_registry.clone(),
                target_registry.clone(),
                task_registry.clone(),
                task_results.clone(),
            )
            .unwrap(),
        );
        let ctx = DefaultPlannerContext::new(
            task_registry.clone(),
            target_registry,
            signature_registry,
            task_results,
            rule_store,
            code_manager,
        );

        let goal = Goal::Build;
        let target = Arc::new("./my/file.ex".into());
        let target_id = ctx.target_registry.register_target(&target);
        let target = ConcreteTarget::new(goal, target_id, target, "".into(), ".".into());

        let dep_target = Arc::new("./my/dep.ex".into());
        let dep_target_id = ctx.target_registry.register_target(&dep_target);
        let dep_target =
            ConcreteTarget::new(goal, dep_target_id, dep_target, "".into(), ".".into());

        let env = ExecutionEnvironment::default();
        let dep_spec = ExecutableSpec::builder()
            .goal(Goal::Build)
            .target(dep_target.clone())
            .signature(
                Signature::builder()
                    .name("test_signature")
                    .target(dep_target.clone())
                    .rule("test_rule")
                    .build()
                    .unwrap(),
            )
            .exec_env(env.clone())
            .hash_and_build(&ctx.task_results)
            .unwrap();
        let dep_manifest = ArtifactManifest::default();
        let unreg_task = UnregisteredTask::builder()
            .goal(Goal::Build)
            .target_id(dep_target_id)
            .build()
            .unwrap();
        let dep_task = task_registry.register(unreg_task);

        ctx.task_results
            .add_task_result(dep_task, dep_spec, dep_manifest);

        let sig = Signature::builder()
            .name("test_signature")
            .rule("test_rule")
            .target(target.clone())
            .deps(vec![dep_task])
            .build()
            .unwrap();

        let unreg_task = UnregisteredTask::builder()
            .goal(Goal::Build)
            .target_id(target_id)
            .build()
            .unwrap();
        let task = task_registry.register(unreg_task);

        let mut p: DefaultPlanner<NoopRuleExecutor> = DefaultPlanner::new(ctx).unwrap();
        let flow = p.plan(task, &sig, env).await.unwrap();

        assert_matches!(flow, PlanningFlow::Planned { spec } => {
            assert_eq!(*spec.target(), target);
            assert_eq!(spec.deps().compile_deps().get(0).unwrap().target_id(), dep_target_id);
        });
    }

    #[tokio::test]
    async fn when_missing_dependencies_we_abort() {
        let warp_root = assert_fs::TempDir::new().unwrap();
        let config = Config::builder()
            .warp_root(warp_root.path().to_path_buf())
            .build()
            .unwrap();
        let rule_store = RuleStore::new(&config).into();
        let task_registry = Arc::new(TaskRegistry::new());
        let target_registry = Arc::new(TargetRegistry::new());
        let signature_registry = Arc::new(SignatureRegistry::new());
        let test_matcher_registry = Arc::new(TestMatcherRegistry::new());
        let task_results = Arc::new(TaskResults::new(
            task_registry.clone(),
            target_registry.clone(),
            signature_registry.clone(),
        ));
        let archive_manager = Arc::new(ArchiveManager::new(&config));
        let artifact_store = Arc::new(DefaultStore::new(config.clone(), archive_manager));
        let code_manager = Arc::new(
            CodeManager::new(
                config.clone(),
                artifact_store,
                test_matcher_registry.clone(),
                target_registry.clone(),
                task_registry.clone(),
                task_results.clone(),
            )
            .unwrap(),
        );
        let ctx = DefaultPlannerContext::new(
            task_registry.clone(),
            target_registry,
            signature_registry,
            task_results,
            rule_store,
            code_manager,
        );

        let goal = Goal::Build;
        let target = Arc::new("./my/file.ex".into());
        let target_id = ctx.target_registry.register_target(&target);
        let target = ConcreteTarget::new(goal, target_id, target, "".into(), ".".into());

        let dep_target = Arc::new("./my/dep.ex".into());
        let dep_target_id = ctx.target_registry.register_target(&dep_target);
        let unreg_task = UnregisteredTask::builder()
            .goal(Goal::Build)
            .target_id(dep_target_id)
            .build()
            .unwrap();
        let dep_task = ctx.task_registry.register(unreg_task);
        dbg!(&dep_task);

        let sig = Signature::builder()
            .name("test_signature")
            .rule("test_rule")
            .target(target)
            .deps(vec![dep_task])
            .build()
            .unwrap();

        let sig_id = ctx.signature_registry.register(sig.clone());

        let unreg_task = UnregisteredTask::builder()
            .goal(Goal::Build)
            .target_id(target_id)
            .signature_id(sig_id)
            .build()
            .unwrap();
        let task = ctx.task_registry.register(unreg_task);
        dbg!(&task);

        let env = ExecutionEnvironment::default();

        let mut p: DefaultPlanner<NoopRuleExecutor> = DefaultPlanner::new(ctx).unwrap();
        let flow = p.plan(task, &sig, env).await.unwrap();

        assert_matches!(flow, PlanningFlow::MissingDeps { deps } => {
            dbg!(&deps);
            assert!(!deps.is_empty());
        });
    }
}
