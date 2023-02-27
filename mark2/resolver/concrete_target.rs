use std::path::PathBuf;

use crate::sync::*;

use super::{Goal, Target};

pub struct ConcreteTarget {
    original_target: Target,
    path: PathBuf,
    goal: Goal,
}

impl ConcreteTarget {
    pub fn new(goal: Goal, original_target: Arc<Target>, path: PathBuf, ) -> Self {
        Self {
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
