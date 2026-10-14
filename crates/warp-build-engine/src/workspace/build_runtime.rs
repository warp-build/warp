use super::{WorkDirectory, Workspace};
use crate::action_cache::ActionCache;
use crate::fingerprint::rustc;
use std::path::PathBuf;
use std::sync::OnceLock;
use warp_store::Cas;

pub(super) struct BuildRuntime {
    pub(super) workspace: Workspace,
    pub(super) _work: WorkDirectory,
    pub(super) compiler: OnceLock<std::result::Result<(PathBuf, String), String>>,
    pub(super) action_cache: ActionCache,
    pub(super) cas: Cas,
}

impl BuildRuntime {
    pub(super) fn compiler(&self) -> std::result::Result<&(PathBuf, String), String> {
        self.compiler
            .get_or_init(|| rustc().map_err(|error| error.to_string()))
            .as_ref()
            .map_err(Clone::clone)
    }
}
