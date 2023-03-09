use super::*;
use std::str::FromStr;
use thiserror::*;

#[derive(Default, Debug, Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub enum Goal {
    #[default]
    Build,
    Test,
    Run,
    Fetch,
}

#[derive(Error, Debug)]
pub enum GoalError {
    #[error("Invalid goal {0}. Valid goals are: run, test, and build.")]
    InvalidGoal(String),
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
            // NOTE(@ostera): fetch can be used to download all dependencies on anything
            Goal::Fetch => true,
        }
    }

    pub fn is_runnable(&self) -> bool {
        matches!(self, Goal::Test | Goal::Run)
    }

    pub fn is_run(&self) -> bool {
        matches!(self, Goal::Run)
    }

    pub fn is_build(&self) -> bool {
        matches!(self, Goal::Build)
    }

    pub fn is_test(&self) -> bool {
        matches!(self, Goal::Test)
    }

    pub fn is_fetch(&self) -> bool {
        matches!(self, Goal::Fetch)
    }
}

impl FromStr for Goal {
    type Err = GoalError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "run" => Ok(Self::Run),
            "test" => Ok(Self::Test),
            "build" => Ok(Self::Build),
            "get" => Ok(Self::Fetch),
            _ => Err(GoalError::InvalidGoal(s.into())),
        }
    }
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
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
