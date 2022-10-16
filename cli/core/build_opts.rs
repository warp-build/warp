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
    pub target_filter: TargetFilter,
    pub force_output_promotion: bool,
}