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
    pub goal: Goal,
    pub target_filter: TargetFilter,
    pub concurrency_limit: usize,
    pub experimental_file_mode: bool,
    pub experimental_force_rebuild: bool,
    pub experimental_stream_analyzer_outputs: bool,
}
