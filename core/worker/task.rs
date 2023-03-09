use crate::model::{Goal, TargetId};

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct Task {
    pub goal: Goal,
    pub target_id: TargetId,
}

impl Task {
    pub fn new(goal: Goal, target_id: TargetId) -> Self {
        Self { goal, target_id }
    }

    pub fn run(target_id: TargetId) -> Self {
        Self {
            target_id,
            goal: Goal::Run,
        }
    }

    pub fn build(target_id: TargetId) -> Self {
        Self {
            target_id,
            goal: Goal::Build,
        }
    }

    pub fn test(target_id: TargetId) -> Self {
        Self {
            target_id,
            goal: Goal::Test,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    impl quickcheck::Arbitrary for Task {
        fn arbitrary(g: &mut quickcheck::Gen) -> Self {
            Self {
                target_id: TargetId::arbitrary(g),
                goal: Goal::arbitrary(g),
            }
        }
    }
}
