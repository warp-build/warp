use crate::resolver::*;

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct Task {
    pub goal: Goal,
    pub target: TargetId,
}

impl Task {
    pub fn new(goal: Goal, target: TargetId) -> Self {
        Self { goal, target }
    }

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

#[cfg(test)]
mod tests {
    use super::*;

    impl quickcheck::Arbitrary for Task {
        fn arbitrary(g: &mut quickcheck::Gen) -> Self {
            Self {
                target: TargetId::arbitrary(g),
                goal: Goal::arbitrary(g),
            }
        }
    }
}
