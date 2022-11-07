use super::*;

#[derive(Default, Debug, Copy, Clone)]
pub enum Goal {
    #[default]
    Build,
    Test,
    Run,
}

impl Goal {
    pub fn includes<T>(&self, t: T) -> bool
    where
        T: AsRef<RuleName>,
    {
        match &self {
            Goal::Test => t.as_ref().ends_with("_test"),
            Goal::Run => {
                t.as_ref().ends_with("_script")
                    || t.as_ref().ends_with("_binary")
                    || t.as_ref().ends_with("_executable")
            }
            Goal::Build => !t.as_ref().ends_with("_test"),
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
