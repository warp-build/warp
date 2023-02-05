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

#[derive(Default, Debug, Copy, Clone)]
pub struct Options {
    pub concurrency_limit: usize,
    pub experimental_file_mode: bool,
    pub experimental_force_rebuild: bool,
    pub experimental_regenerate_signatures: bool,
    pub experimental_runtime_input_detection: bool,
    pub experimental_stream_analyzer_outputs: bool,
    pub disable_remote_cache: bool,
    pub goal: Goal,
    pub signature_filter: SignatureFilter,
}

impl Options {
    pub fn with_goal(mut self, goal: Goal) -> Self {
        self.goal = goal;
        self
    }

    pub fn with_signature_filter(mut self, signature_filter: SignatureFilter) -> Self {
        self.signature_filter = signature_filter;
        self
    }
}
