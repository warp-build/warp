use super::{RuleName, TestMatcherId};
use serde::{Deserialize, Serialize};
use std::str::FromStr;
use thiserror::*;

#[derive(
    Default, Debug, Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd, Serialize, Deserialize,
)]
pub enum Goal {
    /// Build is the default goal, which is used to compile, link, produce container images, and
    /// any other task that produces an artifact that is used to either build more software or to
    /// run
    ///software.
    #[default]
    Build,

    Bootstrap,

    Test {
        matcher_id: Option<TestMatcherId>,
    },

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
            Goal::Test { .. } => t.as_ref().ends_with("_test"),
            Goal::Run => {
                t.as_ref().ends_with("_script")
                    || t.as_ref().ends_with("_binary")
                    || t.as_ref().ends_with("_executable")
            }
            Goal::Build => !t.as_ref().ends_with("_test"),
            Goal::Bootstrap => false,
            Goal::Fetch => true,
        }
    }

    pub fn is_bootstrap(&self) -> bool {
        matches!(self, Goal::Bootstrap)
    }

    pub fn is_runnable(&self) -> bool {
        matches!(self, Goal::Test { .. } | Goal::Run)
    }

    pub fn is_run(&self) -> bool {
        matches!(self, Goal::Run)
    }

    pub fn is_build(&self) -> bool {
        matches!(self, Goal::Build)
    }

    pub fn is_test(&self) -> bool {
        matches!(self, Goal::Test { .. })
    }

    pub fn is_fetch(&self) -> bool {
        matches!(self, Goal::Fetch)
    }

    pub fn test_matcher_id(&self) -> Option<TestMatcherId> {
        if let Goal::Test { matcher_id } = &self {
            return *matcher_id;
        }
        None
    }

    pub fn from_rule_name(rule: &str) -> Goal {
        if rule.ends_with("_test") {
            return Goal::Test { matcher_id: None };
        }
        Goal::Build
    }
}

impl FromStr for Goal {
    type Err = GoalError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "run" => Ok(Self::Run),
            "test" => Ok(Self::Test { matcher_id: None }),
            "build" => Ok(Self::Build),
            "get" => Ok(Self::Fetch),
            _ => Err(GoalError::InvalidGoal(s.into())),
        }
    }
}

impl ToString for Goal {
    fn to_string(&self) -> String {
        match self {
            Self::Run => "run",
            Self::Test { .. } => "test",
            Self::Build => "build",
            Self::Fetch => "get",
            Self::Bootstrap => "bootstrap",
        }
        .to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    impl quickcheck::Arbitrary for Goal {
        fn arbitrary(g: &mut quickcheck::Gen) -> Self {
            g.choose(&[
                Self::Build,
                Self::Test { matcher_id: None },
                Self::Run,
                Self::Fetch,
            ])
            .unwrap()
            .to_owned()
        }
    }
}
