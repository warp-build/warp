use super::*;
use fxhash::*;
use seahash::SeaHasher;
use std::fs::File;
use std::hash::{Hash, Hasher};
use std::io::{BufReader, Read};
use std::path::PathBuf;

/// An ExecutableTarget is a self-contained description of how to build a target in a workspace.
///
pub struct ExecutableTarget {
    /// A vector of actions to be taken _in order_ to produce the target's outputs
    pub actions: Vec<Action>,

    /// The dependencies this target needs to have in place
    pub deps: FxHashSet<Dependency>,

    pub hash: String,
    pub outs: FxHashSet<PathBuf>,
    pub run_script: Option<RunScript>,
    pub srcs: FxHashSet<PathBuf>,
    pub target: Target,
    pub transitive_deps: FxHashSet<Dependency>,
}

pub enum ExecutableTargetError {
    ConflictingOutputs { outputs: Vec<PathBuf> },
}

impl ExecutableTarget {
    pub fn new(
        env: &ExecutionEnvironment,
        target: &Target,
        exec_res: ExecutionResult,
    ) -> Result<Self, ExecutableTargetError> {
        let mut target = todo!();

        target.ensure_outputs_are_safe()?;
        target.recompute_hash(env);

        Ok(target)
    }

    fn recompute_hash(&mut self, env: &ExecutionEnvironment) {
        let mut s = SeaHasher::new();

        self.target.label().hash(&mut s);

        let deps = self.deps.iter().map(|d| d.hash.as_str());

        let actions: Vec<String> = self.actions.iter().map(|a| format!("{:?}", a)).collect();

        let mut srcs: Vec<&PathBuf> = self.srcs.iter().collect();
        srcs.dedup_by(|a, b| a == b);
        srcs.sort();

        let mut seeds: Vec<&str> = {
            deps.chain(self.outs.iter().map(|o| o.to_str().unwrap()))
                .chain(actions.iter().map(|a| a.as_str()))
                .chain(srcs.iter().map(|s| s.to_str().unwrap()))
                .collect()
        };

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

        if !self.target.is_portable() {
            env.hash(&mut s);
        }

        let hash = s.finish();
        self.hash = hash.to_string();
    }

    fn ensure_outputs_are_safe(&mut self) -> Result<(), ComputedTargetError> {
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
}

mod tests {}
