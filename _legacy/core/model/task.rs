use super::{Goal, SignatureId, TargetId, TaskId};
use serde::{Deserialize, Serialize};

#[derive(
    Builder,
    Clone,
    Copy,
    Debug,
    Default,
    Deserialize,
    Eq,
    Hash,
    Ord,
    PartialEq,
    PartialOrd,
    Serialize,
)]
pub struct UnregisteredTask {
    goal: Goal,
    target_id: TargetId,
    #[builder(default, setter(strip_option))]
    signature_id: Option<SignatureId>,
}

impl UnregisteredTask {
    pub fn builder() -> UnregisteredTaskBuilder {
        Default::default()
    }

    pub fn goal(&self) -> Goal {
        self.goal
    }

    pub fn target_id(&self) -> TargetId {
        self.target_id
    }

    pub fn signature_id(&self) -> Option<SignatureId> {
        self.signature_id
    }
}

/// A Task represents a unit of work for the Workers. Tasks are cheap, copyable pieces of data that
/// point to a specific build goal (Build, Run, Test, etc), a specific file that is being built
/// (the [Target]), and a specific [Signature] used to build this file.
///
#[derive(
    Builder,
    Clone,
    Copy,
    Debug,
    Default,
    Deserialize,
    Eq,
    Hash,
    Ord,
    PartialEq,
    PartialOrd,
    Serialize,
)]
pub struct Task {
    id: TaskId,

    goal: Goal,

    target_id: TargetId,

    #[builder(default)]
    signature_id: Option<SignatureId>,
}

impl Task {
    pub fn builder() -> TaskBuilder {
        Default::default()
    }

    pub fn id(&self) -> &TaskId {
        &self.id
    }

    pub fn goal(&self) -> Goal {
        self.goal
    }

    pub fn target_id(&self) -> TargetId {
        self.target_id
    }

    pub fn signature_id(&self) -> Option<SignatureId> {
        self.signature_id
    }

    pub fn set_id(&mut self, id: TaskId) {
        self.id = id;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    impl quickcheck::Arbitrary for UnregisteredTask {
        fn arbitrary(g: &mut quickcheck::Gen) -> Self {
            Self {
                target_id: TargetId::arbitrary(g),
                goal: Goal::arbitrary(g),
                signature_id: None,
            }
        }
    }

    impl quickcheck::Arbitrary for Task {
        fn arbitrary(g: &mut quickcheck::Gen) -> Self {
            Self {
                id: TaskId::next(),
                target_id: TargetId::arbitrary(g),
                goal: Goal::arbitrary(g),
                signature_id: None,
            }
        }
    }
}
