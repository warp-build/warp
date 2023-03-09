use super::*;

#[derive(Default, Debug, Copy, Clone)]
pub enum TargetFilter {
    OnlyTests,
    #[default]
    Everything,
}

impl TargetFilter {
    pub fn passes(&self, target: &Target) -> bool {
        match self {
            TargetFilter::OnlyTests => target.rule_name.ends_with("_test"),
            TargetFilter::Everything => true,
        }
    }
}

#[derive(Default, Debug, Copy, Clone)]
pub struct BuildOpts {
    pub concurrency_limit: usize,
    pub experimental_file_mode: bool,
    pub experimental_force_rebuild: bool,
    pub experimental_regenerate_signatures: bool,
    pub experimental_runtime_input_detection: bool,
    pub experimental_stream_analyzer_outputs: bool,
    pub disable_remote_cache: bool,
    pub goal: Goal,
    pub target_filter: TargetFilter,
}

impl BuildOpts {
    pub fn with_goal(mut self, goal: Goal) -> Self {
        self.goal = goal;
        self
    }

    pub fn with_target_filter(mut self, target_filter: TargetFilter) -> Self {
        self.target_filter = target_filter;
        self
    }
}
