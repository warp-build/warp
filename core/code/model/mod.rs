use crate::model::rule::Config;
use crate::model::{ConcreteTarget, Signature, Task, UnregisteredTask};
use crate::resolver::TargetRegistry;
use crate::testing::TestMatcherRegistry;
use crate::worker::TaskRegistry;
use crate::{Goal, Target};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum GoalModel {
    Build,
    Bootstrap,
    Test(Vec<String>),
    Run,
    Fetch,
}

impl GoalModel {
    pub fn from_goal(goal: Goal, test_matcher_registry: &TestMatcherRegistry) -> GoalModel {
        match goal {
            Goal::Bootstrap => Self::Bootstrap,
            Goal::Build => Self::Build,
            Goal::Fetch => Self::Fetch,
            Goal::Run => Self::Run,
            Goal::Test { matcher_id: None } => Self::Test(vec![]),
            Goal::Test {
                matcher_id: Some(matcher_id),
            } => {
                let matchers = test_matcher_registry.get(matcher_id).raw().to_vec();
                Self::Test(matchers)
            }
        }
    }

    pub fn to_goal(&self, test_matcher_registry: &TestMatcherRegistry) -> Goal {
        match self {
            GoalModel::Build => Goal::Build,
            GoalModel::Bootstrap => Goal::Bootstrap,
            GoalModel::Run => Goal::Run,
            GoalModel::Fetch => Goal::Fetch,
            GoalModel::Test(pats) if pats.is_empty() => Goal::Test { matcher_id: None },
            GoalModel::Test(pats) => {
                let matcher_id = test_matcher_registry.register(pats.to_vec());
                Goal::Test {
                    matcher_id: Some(matcher_id),
                }
            }
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConcreteTargetModel {
    goal: GoalModel,
    original_target: String,
    path: String,
    workspace_root: String,
    deps: Vec<String>,
}

impl ConcreteTargetModel {
    pub fn from_concrete_target(
        target: &ConcreteTarget,
        test_matcher_registry: &TestMatcherRegistry,
    ) -> Self {
        let goal = GoalModel::from_goal(target.goal(), test_matcher_registry);

        Self {
            goal,
            original_target: target.original_target().to_string(),
            path: target.path().to_string_lossy().to_string(),
            workspace_root: target.workspace_root().to_string_lossy().to_string(),
            deps: vec![],
        }
    }

    pub fn to_concrete_target(
        self,
        test_matcher_registry: &TestMatcherRegistry,
        target_registry: &TargetRegistry,
    ) -> ConcreteTarget {
        let goal = GoalModel::to_goal(&self.goal, test_matcher_registry);
        let target: Target = self.original_target.into();
        let target_id = target_registry.register_target(target.clone());

        ConcreteTarget::builder()
            .goal(goal)
            .target_id(target_id)
            .target(target.into())
            .path(self.path)
            .workspace_root(self.workspace_root)
            .build()
            .unwrap()
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TargetModel {
    goal: GoalModel,
    target: String,
}

impl TargetModel {
    pub fn from_target(target: &Target, goal: GoalModel) -> Self {
        Self {
            goal,
            target: target.to_string(),
        }
    }

    pub fn to_task(
        self,
        test_matcher_registry: &TestMatcherRegistry,
        target_registry: &TargetRegistry,
        task_registry: &TaskRegistry,
    ) -> Task {
        let goal = GoalModel::to_goal(&self.goal, test_matcher_registry);
        let target: Target = self.target.into();
        let target_id = target_registry.register_target(target);

        let unreg_task = UnregisteredTask::builder()
            .goal(goal)
            .target_id(target_id)
            .build()
            .unwrap();

        task_registry.register(unreg_task)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SignatureModel {
    name: String,
    target: ConcreteTargetModel,
    rule: String,
    deps: Vec<TargetModel>,
    runtime_deps: Vec<TargetModel>,
    config: Config,
}

impl SignatureModel {
    pub fn from_signature(
        sig: &Signature,
        test_matcher_registry: &TestMatcherRegistry,
        target_registry: &TargetRegistry,
    ) -> SignatureModel {
        let target = ConcreteTargetModel::from_concrete_target(sig.target(), test_matcher_registry);

        let mut deps = vec![];
        for dep in sig.deps() {
            let goal = GoalModel::from_goal(dep.goal(), test_matcher_registry);
            let t = target_registry.get_target(dep.target_id());
            let target = TargetModel::from_target(&t, goal);
            deps.push(target);
        }

        let mut runtime_deps = vec![];
        for dep in sig.runtime_deps() {
            let goal = GoalModel::from_goal(dep.goal(), test_matcher_registry);
            let t = target_registry.get_target(dep.target_id());
            let target = TargetModel::from_target(&t, goal);
            runtime_deps.push(target);
        }

        Self {
            name: sig.name().to_string(),
            target,
            rule: sig.rule().to_string(),
            deps,
            runtime_deps,
            config: sig.config().clone(),
        }
    }

    pub fn to_signature(
        self,
        test_matcher_registry: &TestMatcherRegistry,
        target_registry: &TargetRegistry,
        task_registry: &TaskRegistry,
    ) -> Signature {
        let mut deps = vec![];
        for dep in self.deps {
            deps.push(TargetModel::to_task(
                dep,
                test_matcher_registry,
                target_registry,
                task_registry,
            ));
        }

        let mut runtime_deps = vec![];
        for dep in self.runtime_deps {
            runtime_deps.push(TargetModel::to_task(
                dep,
                test_matcher_registry,
                target_registry,
                task_registry,
            ));
        }

        let target = ConcreteTargetModel::to_concrete_target(
            self.target,
            test_matcher_registry,
            target_registry,
        );

        Signature::builder()
            .name(self.name)
            .rule(self.rule)
            .target(target)
            .deps(deps)
            .runtime_deps(runtime_deps)
            .config(self.config)
            .build()
            .unwrap()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[quickcheck]
    fn to_and_from_model_preserves_data(
        goal: Goal,
        name: String,
        rule: String,
        // config: Config,
    ) {
        let workspace_root = assert_fs::TempDir::new().unwrap();

        let test_matcher_registry = TestMatcherRegistry::new();
        let target_registry = TargetRegistry::new();
        let task_registry = TaskRegistry::new();

        let target_path = "test/file.rs";
        let target: Target = target_path.to_string().into();
        let target_id = target_registry.register_target(target.clone());

        let final_path = workspace_root.path().join(target_path);

        let ct = ConcreteTarget::builder()
            .goal(goal)
            .target_id(target_id)
            .target(target.into())
            .path(final_path)
            .workspace_root(workspace_root.path().to_path_buf())
            .build()
            .unwrap();

        let deps = vec![{
            let unreg_task = UnregisteredTask::builder()
                .goal(goal)
                .target_id(target_id)
                .build()
                .unwrap();
            task_registry.register(unreg_task)
        }];

        let runtime_deps = vec![{
            let unreg_task = UnregisteredTask::builder()
                .goal(goal)
                .target_id(target_id)
                .build()
                .unwrap();
            task_registry.register(unreg_task)
        }];

        let sig = Signature::builder()
            .name(name)
            .rule(rule)
            .target(ct)
            .deps(deps)
            .runtime_deps(runtime_deps)
            // .config(config)
            .build()
            .unwrap();

        let model = SignatureModel::from_signature(&sig, &test_matcher_registry, &target_registry);
        let sig2 = model.to_signature(&test_matcher_registry, &target_registry, &task_registry);

        assert_eq!(sig, sig2);
    }
}
