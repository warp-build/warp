use super::{BuildDependency, Label, TargetKind};
use crate::graph::TaskId;
use std::path::PathBuf;
use std::sync::{Arc, OnceLock};

/// A build-graph node with a user-facing label and compilation inputs.
pub struct BuildTarget {
    pub(super) label: Label,
    pub(super) id: TaskId,
    pub(super) package_id: String,
    pub(super) package_name: String,
    pub(super) package_version: String,
    pub(super) package_root: PathBuf,
    pub(super) source_root: PathBuf,
    pub(super) source_fingerprint: Arc<OnceLock<std::result::Result<String, String>>>,
    pub(super) source_file: PathBuf,
    pub(super) target_name: String,
    pub(super) crate_name: String,
    pub(super) edition: String,
    pub(super) features: Vec<String>,
    pub(super) kind: TargetKind,
    pub(super) dependencies: Vec<BuildDependency>,
    pub(super) compiler_artifact_name: String,
    pub(super) output: PathBuf,
}

impl BuildTarget {
    pub fn label(&self) -> &Label {
        &self.label
    }
}
