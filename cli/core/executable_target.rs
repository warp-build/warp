use super::*;
use fxhash::*;
use seahash::SeaHasher;
use std::fs::File;
use std::hash::{Hash, Hasher};
use std::io::{BufReader, Read};
use std::path::PathBuf;
use thiserror::*;

/// An ExecutableTarget is a self-contained description of how to build a target in a workspace.
///
#[derive(Debug, Clone)]
pub struct ExecutableTarget {
    pub label: Label,

    /// A vector of actions to be taken _in order_ to produce the target's outputs
    pub actions: Vec<Action>,

    /// The dependencies this target needs to have in place
    pub deps: FxHashSet<Dependency>,

    pub hash: String,

    pub outs: FxHashSet<PathBuf>,

    pub run_script: Option<RunScript>,

    pub provides: FxHashMap<String, PathBuf>,

    pub srcs: FxHashSet<PathBuf>,

    pub transitive_deps: FxHashSet<Dependency>,

    pub rule: Rule,
}

#[derive(Error, Debug)]
pub enum ExecutableTargetError {
    #[error("The following outputs conflict with dependency outputs: {outputs:?}")]
    ConflictingOutputs { outputs: Vec<PathBuf> },
}

impl ExecutableTarget {
    pub async fn new(
        env: &ExecutionEnvironment,
        rule: &Rule,
        target: &Target,
        deps: &[Dependency],
        transitive_deps: &[Dependency],
        exec_result: ExecutionResult,
    ) -> Result<Self, ExecutableTargetError> {
        let mut this = Self {
            actions: exec_result.actions,
            deps: deps.iter().cloned().collect(),
            hash: "".to_string(),
            label: target.label.clone(),
            outs: exec_result.outs,
            rule: rule.clone(),
            run_script: exec_result.run_script,
            srcs: exec_result.srcs,
            transitive_deps: transitive_deps.iter().cloned().collect(),
            provides: exec_result.provides,
        };

        this.ensure_outputs_are_safe()?;
        this.recompute_hash(env).await;

        Ok(this)
    }

    pub fn to_dependency(&self) -> Dependency {
        Dependency {
            rule_name: self.rule.name.clone(),
            label: self.label.clone(),
            hash: self.hash.clone(),
            outs: self.outs.iter().cloned().collect(),
            srcs: self.srcs.iter().cloned().collect(),
        }
    }

    async fn recompute_hash(&mut self, env: &ExecutionEnvironment) {
        let mut s = SeaHasher::new();

        self.label.hash(&mut s);

        let deps = self.deps.iter().map(|d| d.hash.as_str());

        let actions: Vec<String> = self.actions.iter().map(|a| format!("{:?}", a)).collect();

        let mut srcs: Vec<&PathBuf> = self.srcs.iter().collect();
        srcs.dedup_by(|a, b| a == b);
        srcs.sort();

        let mut seeds: Vec<&str> = deps
            .chain(self.outs.iter().map(|o| o.to_str().unwrap()))
            .chain(actions.iter().map(|a| a.as_str()))
            .chain(srcs.iter().map(|s| s.to_str().unwrap()))
            .collect();

        seeds.dedup_by(|a, b| a == b);
        seeds.sort_unstable();

        for seed in seeds {
            seed.hash(&mut s);
        }

        for src in srcs {
            let f = File::open(&src).unwrap_or_else(|_| panic!("Unable to open: {:?}", &src));
            let mut buffer = [0; 2048];
            let mut reader = BufReader::new(f);
            while let Ok(len) = reader.read(&mut buffer) {
                if len == 0 {
                    break;
                }
                buffer[..len].hash(&mut s);
            }
        }

        if !self.is_portable() {
            env.hash(&mut s);
        }

        let hash = s.finish();
        self.hash = hash.to_string();
    }

    fn ensure_outputs_are_safe(&mut self) -> Result<(), ExecutableTargetError> {
        let output_set: FxHashSet<PathBuf> = self.outs.iter().cloned().collect();

        let dep_output_set: FxHashSet<PathBuf> =
            self.deps.iter().flat_map(|os| os.outs.clone()).collect();

        if !output_set.is_disjoint(&dep_output_set) {
            let outputs = output_set
                .intersection(&dep_output_set)
                .cloned()
                .collect::<Vec<PathBuf>>();
            Err(ExecutableTargetError::ConflictingOutputs { outputs })
        } else {
            Ok(())
        }
    }

    pub fn is_portable(&self) -> bool {
        self.rule.portability == Portability::Portable
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::*;

    fn rule() -> Rule {
        Rule::new(
            "test_rule".to_string(),
            "TestRule".to_string(),
            vec![],
            ConfigSpec::default(),
            RuleConfig::default(),
            Runnable::NotRunnable,
            Pinned::Pinned,
            Portability::Portable,
        )
    }

    async fn target(label: Label) -> ExecutableTarget {
        let rule = rule();
        let cfg = RuleConfig::default();
        let target = Target::new(label, &rule.name, cfg);
        ExecutableTarget::new(
            &ExecutionEnvironment::new(),
            &rule,
            &target,
            &[],
            &[],
            ExecutionResult::default(),
        )
        .await
        .unwrap()
    }

    #[tokio::test]
    async fn can_be_turned_into_a_dependency() {
        let t = target(Label::new("test")).await;
        let d = t.to_dependency();

        assert_eq!(d.rule_name, t.rule.name);
        assert_eq!(d.label, t.label);
        assert_eq!(d.hash, t.hash);
        assert_eq!(d.outs, t.outs.iter().cloned().collect::<Vec<PathBuf>>());
        assert_eq!(d.srcs, t.srcs.iter().cloned().collect::<Vec<PathBuf>>());
    }

    #[tokio::test]
    async fn preserves_target_information() {
        let l = Label::new("test");

        let rule = rule();
        let target = ExecutableTarget::new(
            &ExecutionEnvironment::new(),
            &rule,
            &Target::new(l.clone(), &rule.name, RuleConfig::default()),
            &[],
            &[],
            ExecutionResult::default(),
        )
        .await
        .unwrap();

        assert_eq!(target.label, l);
    }

    #[tokio::test]
    async fn preserves_results_from_rule_execution() {
        let l = Label::new("test");

        let actions = vec![];
        let outs = vec![].iter().cloned().collect::<FxHashSet<PathBuf>>();
        let srcs = vec![].iter().cloned().collect::<FxHashSet<PathBuf>>();
        let run_script = Some(RunScript {
            env: HashMap::default(),
            run_script: PathBuf::from("run"),
        });

        let rule = rule();
        let target = ExecutableTarget::new(
            &ExecutionEnvironment::new(),
            &rule,
            &Target::new(l, &rule.name, RuleConfig::default()),
            &[],
            &[],
            ExecutionResult {
                actions: actions.clone(),
                outs: outs.clone(),
                srcs: srcs.clone(),
                run_script: run_script.clone(),
                provides: FxHashMap::default(),
            },
        )
        .await
        .unwrap();

        assert_eq!(target.actions, actions);
        assert_eq!(target.outs, outs);
        assert_eq!(target.srcs, srcs);
        assert_eq!(target.run_script, run_script);
    }

    #[tokio::test]
    async fn ensures_outputs_are_safe_on_creation() {
        let l = Label::new("test");
        let rule = rule();

        let conflicting_output = PathBuf::from("conflicting-file");

        let deps = vec![Dependency {
            rule_name: "rule-name".to_string(),
            label: Label::new("dep"),
            hash: "".to_string(),
            outs: vec![conflicting_output.clone()],
            srcs: vec![],
        }];

        let result = ExecutableTarget::new(
            &ExecutionEnvironment::new(),
            &rule,
            &Target::new(l, &rule.name, RuleConfig::default()),
            &deps,
            &[],
            ExecutionResult {
                actions: vec![],
                outs: vec![conflicting_output.clone()]
                    .iter()
                    .cloned()
                    .collect::<FxHashSet<PathBuf>>(),
                srcs: FxHashSet::default(),
                run_script: None,
                provides: FxHashMap::default(),
            },
        )
        .await;

        assert!(matches!(
            result,
            Err(ExecutableTargetError::ConflictingOutputs { outputs }) if outputs == vec![conflicting_output.clone()]
        ));
    }
}
