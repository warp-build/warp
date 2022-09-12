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
            hash: "0".to_string(),
            label: target.label.clone(),
            outs: exec_result.outs,
            rule: rule.clone(),
            run_script: exec_result.run_script,
            srcs: exec_result.srcs,
            transitive_deps: transitive_deps.iter().cloned().collect(),
        };

        this.ensure_outputs_are_safe()?;
        this.recompute_hash(env).await;

        Ok(this)
    }

    pub fn to_dependency(&self) -> Dependency {
        Dependency {
            is_pinned: false,
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
            let f = File::open(&src).expect(&format!("Unable to open: {:?}", &src));
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

    pub fn is_pinned(&self) -> bool {
        self.rule.pinned == Pinned::Pinned
    }
}

#[cfg(test)]
mod tests {}
