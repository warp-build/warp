use super::{
    ConcreteTarget, Dependencies, ExecutionEnvironment, Goal, Portability, ProvidedFiles,
    Signature, SourceKind, SourceSet,
};
use crate::executor::actions::Action;
use crate::store::ArtifactId;
use crate::util::serde::*;
use crate::worker::TaskResults;
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::collections::BTreeMap;
use std::fs::File;
use std::io::{BufReader, Read};
use std::path::PathBuf;
use thiserror::Error;

#[derive(Builder, Debug, Serialize, Deserialize)]
#[builder(build_fn(error = "ExecutableSpecError", name = "inner_build"))]
pub struct ExecutableSpec {
    goal: Goal,

    target: ConcreteTarget,

    signature: Signature,

    #[builder(setter(skip))]
    hash: String,

    #[builder(default)]
    actions: Vec<Action>,

    #[builder(default)]
    srcs: SourceSet,

    #[builder(default)]
    outs: SourceSet,

    #[builder(default)]
    shell_env: BTreeMap<String, String>,

    exec_env: ExecutionEnvironment,

    #[builder(default)]
    provides: ProvidedFiles,

    #[builder(default)]
    deps: Dependencies,

    #[serde(with = "iso8601")]
    #[builder(default)]
    planning_start_time: DateTime<Utc>,

    #[serde(with = "iso8601")]
    #[builder(default)]
    planning_end_time: DateTime<Utc>,

    #[builder(default)]
    portability: Portability,
}

impl ExecutableSpec {
    pub fn builder() -> ExecutableSpecBuilder {
        Default::default()
    }

    /// The goal that this spec achieves
    pub fn goal(&self) -> Goal {
        self.goal
    }

    /// The ID this Artifact will have once its build.
    pub fn artifact_id(&self) -> ArtifactId {
        ArtifactId::new(&self.hash)
    }

    /// When this target planning started.
    pub fn planning_start_time(&self) -> DateTime<Utc> {
        self.planning_start_time
    }

    /// When this target planning ended.
    pub fn planning_end_time(&self) -> DateTime<Utc> {
        self.planning_end_time
    }

    /// The target this spec will build.
    pub fn target(&self) -> &ConcreteTarget {
        &self.target
    }

    /// The required dependencies to build this spec and run any output artifacts.
    pub fn deps(&self) -> &Dependencies {
        &self.deps
    }

    /// The files provided by this spec to other targets.
    pub fn provides(&self) -> &ProvidedFiles {
        &self.provides
    }

    /// The source files to this spec.
    pub fn srcs(&self) -> &SourceSet {
        &self.srcs
    }

    /// The output files of this spec.
    pub fn outs(&self) -> &SourceSet {
        &self.outs
    }

    /// The shell environment to be used when executing this spec.
    pub fn shell_env(&self) -> &BTreeMap<String, String> {
        &self.shell_env
    }

    /// The unique hash of all the sources, inputs, and actions to this spec.
    pub fn hash(&self) -> &str {
        self.hash.as_ref()
    }

    /// The actions to be executed to create the outputs of this spec. Note that this vector is
    /// ordered and the actions must be carried in order.
    pub fn actions(&self) -> &[Action] {
        &self.actions
    }

    pub fn exec_env(&self) -> &ExecutionEnvironment {
        &self.exec_env
    }

    /// Whether this target is portable across architectures or not.
    pub fn portability(&self) -> &Portability {
        &self.portability
    }

    pub fn signature(&self) -> &Signature {
        &self.signature
    }

    pub fn set_signature(&mut self, signature: Signature) {
        self.signature = signature;
    }

    pub fn set_target(&mut self, target: ConcreteTarget) {
        self.target = target;
    }
}

impl ExecutableSpecBuilder {
    pub fn hash_and_build(
        &mut self,
        task_results: &TaskResults,
    ) -> Result<ExecutableSpec, ExecutableSpecError> {
        let mut spec = self.inner_build()?;

        let mut s = Sha256::new();

        let actions: Vec<String> = spec.actions().iter().map(|a| format!("{:?}", a)).collect();

        let mut srcs: Vec<PathBuf> = spec.srcs().files().to_vec();

        srcs.dedup_by(|a, b| a == b);
        srcs.sort();

        let deps: Vec<String> = spec
            .deps()
            .compile_deps()
            .iter()
            .chain(spec.deps().transitive_deps().iter())
            .chain(spec.deps().toolchains().iter())
            .map(|d| {
                task_results
                    .get_task_result(d)
                    .unwrap()
                    .artifact_manifest
                    .hash()
                    .to_string()
            })
            .collect();

        let outs = spec.outs().files();
        let mut seeds: Vec<&str> = deps
            .iter()
            .map(|d| d.as_str())
            .chain(outs.iter().map(|o| o.to_str().unwrap()))
            .chain(actions.iter().map(|a| a.as_str()))
            .chain(srcs.iter().map(|s| s.to_str().unwrap()))
            .collect();

        seeds.dedup_by(|a, b| a == b);
        seeds.sort_unstable();

        for seed in seeds {
            s.update(seed.as_bytes());
        }

        let target = self.target.as_ref().unwrap();
        let root = target.workspace_root();

        let mut srcs = spec.srcs().sources().iter().collect::<Vec<&SourceKind>>();
        srcs.sort();
        for src in srcs {
            match src {
                SourceKind::Chunk(_, chunk) => {
                    s.update(chunk);
                }
                SourceKind::File(src) => {
                    let src = root.join(src);
                    let f =
                        File::open(&src).unwrap_or_else(|_| panic!("Unable to open: {:?}", &src));
                    let mut buffer = [0; 2048];
                    let mut reader = BufReader::new(f);
                    while let Ok(len) = reader.read(&mut buffer) {
                        if len == 0 {
                            break;
                        }
                        s.update(&buffer[..len]);
                    }
                }
            }
        }

        if let Portability::ArchitectureDependent = spec.portability() {
            s.update(spec.exec_env().host_triple.as_bytes());
        }

        spec.hash = format!("{:x}", s.finalize());

        Ok(spec)
    }
}

#[derive(Error, Debug)]
pub enum ExecutableSpecError {
    #[error(transparent)]
    BuilderError(derive_builder::UninitializedFieldError),
}

impl From<derive_builder::UninitializedFieldError> for ExecutableSpecError {
    fn from(value: derive_builder::UninitializedFieldError) -> Self {
        ExecutableSpecError::BuilderError(value)
    }
}
