use crate::resolver::*;

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct Task {
    pub target: TargetId,
    pub goal: Goal,
}

impl Task {
    pub fn run(target: TargetId) -> Self {
        Self {
            target,
            goal: Goal::Run,
        }
    }

    pub fn build(target: TargetId) -> Self {
        Self {
            target,
            goal: Goal::Build,
        }
    }

    pub fn test(target: TargetId) -> Self {
        Self {
            target,
            goal: Goal::Test,
        }
    }
}
