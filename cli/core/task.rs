use super::*;

#[derive(Default, Debug, Copy, Clone)]
pub enum Goal {
    #[default]
    Build,
    Test,
    Run,
}

impl Goal {
    pub fn includes(&self, target: &Target) -> bool {
        match &self {
            Goal::Test => target.rule_name.ends_with("_test"),
            Goal::Build | Goal::Run => !target.rule_name.ends_with("_test"),
        }
    }

    pub fn is_runnable(&self) -> bool {
        matches!(self, Goal::Test | Goal::Run)
    }

    pub fn is_run(&self) -> bool {
        matches!(self, Goal::Run)
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Task {
    pub label: LabelId,
    pub goal: Goal,
}

impl Task {
    pub fn run(label: LabelId) -> Self {
        Self {
            label,
            goal: Goal::Run,
        }
    }
    pub fn build(label: LabelId) -> Self {
        Self {
            label,
            goal: Goal::Build,
        }
    }
    pub fn test(label: LabelId) -> Self {
        Self {
            label,
            goal: Goal::Test,
        }
    }
}
