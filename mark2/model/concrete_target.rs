use super::{Goal, Target, TargetId};
use crate::sync::*;
use std::path::PathBuf;

/// A ConcreteTarget is a target that has gone through the first phase of resolution.
///
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ConcreteTarget {
    original_target: Arc<Target>,
    path: PathBuf,
    goal: Goal,
    deps: Vec<TargetId>,
}

impl ConcreteTarget {
    pub fn new(goal: Goal, original_target: Arc<Target>, path: PathBuf) -> Self {
        Self {
            deps: original_target.deps().to_vec(),
            original_target,
            path,
            goal,
        }
    }

    pub fn original_target(&self) -> Arc<Target> {
        self.original_target.clone()
    }

    pub fn path(&self) -> &PathBuf {
        &self.path
    }
}

impl ToString for ConcreteTarget {
    fn to_string(&self) -> String {
        self.original_target.to_string()
    }
}
