use super::*;
use fxhash::*;
use sha2::{Digest, Sha256};
use std::fs::File;
use std::io::{BufReader, Read};
use std::path::PathBuf;
use thiserror::*;

/// An ExecutableTarget is a self-contained description of how to build a target in a workspace.
///
#[derive(Default, Debug, Clone)]
pub struct ExecutableTarget {
    pub target_plan_started_at: chrono::DateTime<chrono::Utc>,
    pub target_plan_ended_at: chrono::DateTime<chrono::Utc>,

    pub label: LocalLabel,
    pub hash: String,
    pub rule: Rule,

    /// A vector of actions to be taken _in order_ to produce the target's outputs
    pub actions: Vec<Action>,

    /// The dependencies this target needs to have in place
    pub deps: Vec<LabelId>,

    pub transitive_deps: Vec<LabelId>,

    /// Transitive Runtime Dependencies needed to execute this target
    pub runtime_deps: Vec<LabelId>,

    /// Dependencies that need to be present but not copied into the cache
    pub toolchains: Vec<LabelId>,

    pub srcs: FxHashSet<SourceInput>,

    pub outs: FxHashSet<PathBuf>,

    pub run_script: Option<RunScript>,

    pub provides: FxHashMap<String, PathBuf>,

    pub env: FxHashMap<String, String>,
}

#[derive(Error, Debug)]
pub enum ExecutableTargetError {
    #[error("The following outputs conflict with dependency outputs: {outputs:?}")]
    ConflictingOutputs { outputs: Vec<PathBuf> },

    #[error("Can not create executable target with non-local label: {label:#?}")]
    NonLocalLabel { label: Label },
}

impl ExecutableTarget {
    pub async fn new(
        env: &ExecutionEnvironment,
        rule: &Rule,
        target: &Target,
        deps: &[LabelId],
        runtime_deps: &[LabelId],
        transitive_deps: &[LabelId],
        toolchains: &[LabelId],
        exec_result: ExecutionResult,
        build_results: &BuildResults,
        label_registry: &LabelRegistry,
    ) -> Result<Self, ExecutableTargetError> {
        let label = target
            .label
            .get_local()
            .ok_or_else(|| ExecutableTargetError::NonLocalLabel {
                label: target.label.clone(),
            })?
            .to_owned();

        let mut this = Self {
            target_plan_ended_at: exec_result.target_plan_ended_at,
            target_plan_started_at: exec_result.target_plan_started_at,
            actions: exec_result.actions,
            deps: deps.to_vec(),
            runtime_deps: runtime_deps.to_vec(),
            hash: "".to_string(),
            label,
            outs: exec_result.outs,
            rule: rule.clone(),
            run_script: exec_result.run_script,
            srcs: exec_result.srcs,
            transitive_deps: transitive_deps.to_vec(),
            toolchains: toolchains.to_vec(),
            provides: exec_result.provides,
            env: exec_result.env,
        };

        this.ensure_outputs_are_safe(build_results)?;
        this.recompute_hash(env, build_results, label_registry)
            .await;

        Ok(this)
    }

    pub fn placeholder(label: LocalLabel) -> Self {
        Self {
            label,
            ..Default::default()
        }
    }

    async fn recompute_hash(
        &mut self,
        env: &ExecutionEnvironment,
        build_results: &BuildResults,
        label_registry: &LabelRegistry,
    ) {
        let mut s = Sha256::new();

        s.update(self.label.to_string().as_bytes());

        let actions: Vec<String> = self.actions.iter().map(|a| format!("{:?}", a)).collect();

        let mut srcs: Vec<PathBuf> = self.srcs.iter().map(|src| src.path()).collect();
        srcs.dedup_by(|a, b| a == b);
        srcs.sort();

        let deps: Vec<String> = self
            .deps
            .iter()
            .chain(self.transitive_deps.iter())
            .chain(self.toolchains.iter())
            .flat_map(|d| build_results.get_manifest(*d))
            .map(|d| d.hash.to_owned())
            .collect();

        let mut seeds: Vec<&str> = deps
            .iter()
            .map(|d| d.as_str())
            .chain(self.outs.iter().map(|o| o.to_str().unwrap()))
            .chain(actions.iter().map(|a| a.as_str()))
            .chain(srcs.iter().map(|s| s.to_str().unwrap()))
            .collect();

        seeds.dedup_by(|a, b| a == b);
        seeds.sort_unstable();

        for seed in seeds {
            s.update(seed.as_bytes());
        }

        for src in &self.srcs {
            if let SourceInput::Chunk(chunk) = src {
                if chunk.symbol.is_named() {
                    s.update(&chunk.source);
                    s.update(&chunk.ast_hash);
                    continue;
                }
            }

            let src = match src {
                SourceInput::Chunk(chunk) => chunk.path.to_path_buf(),
                SourceInput::Path(src) => src.to_path_buf(),
            };

            let src = self.label.workspace().join(src);
            let f = File::open(&src).unwrap_or_else(|_| panic!("Unable to open: {:?}", &src));
            let mut buffer = [0; 2048];
            let mut reader = BufReader::new(f);
            while let Ok(len) = reader.read(&mut buffer) {
                if len == 0 {
                    break;
                }
                s.update(&buffer[..len]);
            }
        }

        if !self.is_portable() {
            s.update(env.host_triple.as_bytes());
        }

        self.hash = format!("{:x}", s.finalize());
    }

    fn ensure_outputs_are_safe(
        &mut self,
        build_results: &BuildResults,
    ) -> Result<(), ExecutableTargetError> {
        let output_set: FxHashSet<PathBuf> = self.outs.iter().cloned().collect();

        let dep_output_set: FxHashSet<PathBuf> = self
            .deps
            .iter()
            .flat_map(|l| build_results.get_manifest(*l))
            .flat_map(|os| os.outs.clone())
            .collect();

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

    pub fn srcs(&self) -> Vec<PathBuf> {
        self.srcs.iter().map(|src| src.path()).collect()
    }
}

#[cfg(test)]
mod tests {
    use std::collections::{BTreeMap, HashMap};

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
            &[],
            ExecutionResult::default(),
        )
        .await
        .unwrap()
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
            &[],
            ExecutionResult {
                target_plan_ended_at: chrono::Utc::now(),
                target_plan_started_at: chrono::Utc::now(),
                actions: actions.clone(),
                outs: outs.clone(),
                srcs: srcs.clone(),
                run_script: run_script.clone(),
                provides: FxHashMap::default(),
                env: FxHashMap::default(),
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

        let deps = vec![TargetManifest {
            rule_name: "rule-name".to_string(),
            label: Label::new("dep"),
            hash: "".to_string(),
            outs: vec![conflicting_output.clone()],
            srcs: vec![],
            cached: false,
            is_valid: true,
            provides: BTreeMap::default(),
            deps: BTreeMap::default(),
            transitive_deps: BTreeMap::default(),
            toolchains: BTreeMap::default(),
            env: BTreeMap::default(),
            buildstamps: BuildStamps::default(),
        }];

        let result = ExecutableTarget::new(
            &ExecutionEnvironment::new(),
            &rule,
            &Target::new(l, &rule.name, RuleConfig::default()),
            &deps,
            &[],
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
                env: FxHashMap::default(),
            },
        )
        .await;

        assert!(matches!(
            result,
            Err(ExecutableTargetError::ConflictingOutputs { outputs }) if outputs == vec![conflicting_output.clone()]
        ));
    }
}
