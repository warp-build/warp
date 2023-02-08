use std::path::PathBuf;

use super::*;
use crate::planner::*;
use crate::resolver::*;

#[derive(Default, Debug, Copy, Clone)]
pub enum SignatureFilter {
    OnlyTests,
    #[default]
    Everything,
}

impl SignatureFilter {
    pub fn passes(&self, sig: &Signature) -> bool {
        match self {
            Self::OnlyTests => sig.rule.ends_with("_test"),
            Self::Everything => true,
        }
    }
}

/// A collection of flags and invocation options that can affect how the different subsystems run.
///
/// > NOTE(@ostera): we probably want to split this into smaller option structs that are owned by
/// > different subsystems.
#[derive(Debug, Clone)]
pub struct WarpOptions {
    pub concurrency_limit: usize,
    pub disable_remote_cache: bool,
    pub force_rebuild: bool,
    pub goal: Goal,
    pub regenerate_signatures: bool,
    pub signature_filter: SignatureFilter,
    pub verbose_tricorder: bool,

    pub invocation_dir: PathBuf,
    pub warp_root: PathBuf,
    pub current_user: String,
}

impl Default for WarpOptions {
    fn default() -> Self {
        Self {
            concurrency_limit: Default::default(),
            disable_remote_cache: Default::default(),
            force_rebuild: Default::default(),
            regenerate_signatures: Default::default(),
            verbose_tricorder: Default::default(),
            goal: Default::default(),
            signature_filter: Default::default(),
            current_user: "warp".to_string(),
            invocation_dir: PathBuf::from("."),
            warp_root: PathBuf::from("/warp"),
        }
    }
}

impl WarpOptions {
    pub fn with_goal(mut self, goal: Goal) -> Self {
        self.goal = goal;
        self
    }

    pub fn with_signature_filter(mut self, signature_filter: SignatureFilter) -> Self {
        self.signature_filter = signature_filter;
        self
    }
}
