use super::*;
use fxhash::*;
use std::path::PathBuf;
use thiserror::*;

/// An ExecutableTarget is a self-contained description of how to build a target in a workspace.
///
#[derive(Default, Debug, Clone)]
pub struct ExecutableTarget {
    pub target_plan_started_at: chrono::DateTime<chrono::Utc>,
    pub target_plan_ended_at: chrono::DateTime<chrono::Utc>,

    pub target: FsTarget,
    pub hash: String,
    pub rule: Rule,

    /// A vector of actions to be taken _in order_ to produce the target's outputs
    pub actions: Vec<Action>,

    /// The dependencies this target needs to have in place
    pub deps: Vec<TargetId>,

    pub transitive_deps: Vec<TargetId>,

    /// Transitive Runtime Dependencies needed to execute this target
    pub runtime_deps: Vec<TargetId>,

    /// Dependencies that need to be present but not copied into the cache
    pub toolchains: Vec<TargetId>,

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

    #[error("Can not create executable target with non-local target: {target:#?}")]
    NonFsTarget { target: Target },
}

#[cfg(test)]
mod tests {
    use std::collections::{BTreeMap, HashMap};

    use super::*;

    #[tokio::test]
    async fn preserves_target_information() {}

    #[tokio::test]
    async fn preserves_results_from_rule_execution() {}

    #[tokio::test]
    async fn ensures_outputs_are_safe_on_creation() {}
}
