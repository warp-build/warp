mod config;
pub mod expander;

pub use config::*;
use expander::*;

use crate::Target;

pub type RuleName = String;

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub enum RuleKind {
    #[default]
    Build,
    Run,
    Test,
}

impl RuleKind {
    pub fn is_runnable(&self) -> bool {
        matches!(&self, RuleKind::Run | RuleKind::Test)
    }
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub enum Pinned {
    #[default]
    Pinned,
    Unpinned,
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub enum Portability {
    Portable,
    #[default]
    ArchitectureDependent,
}

/// A Rule defines what actions to take to perform some work.
///
#[derive(Debug, Clone, Default)]
pub struct Rule {
    name: RuleName,
    mnemonic: String,
    toolchains: Vec<Target>,
    config: Spec,
    defaults: Config,
    kind: RuleKind,
    pinned: Pinned,
    portability: Portability,
}

impl Rule {
    /// The name of this rule.
    ///
    /// Rule names are unique in a workspace.
    ///
    pub fn name(&self) -> &str {
        self.name.as_ref()
    }

    /// A pretty name to print while running this rule.
    pub fn mnemonic(&self) -> &str {
        self.mnemonic.as_ref()
    }

    /// The toolchains this tool depends on.
    pub fn toolchains(&self) -> &[Target] {
        self.toolchains.as_ref()
    }

    /// The rule's configuration map.
    ///
    /// These are the things that a user can pass in when configuring their target,
    /// and it will always support at least `name: String`.
    ///
    pub fn config(&self) -> &Spec {
        &self.config
    }

    /// A map of default configuration values.
    pub fn defaults(&self) -> &Config {
        &self.defaults
    }

    /// The kind of rule this is (buildable, kind, testable, etc)
    pub fn kind(&self) -> &RuleKind {
        &self.kind
    }

    /// Whether targets of this rule as pinned or not.
    pub fn pinned(&self) -> &Pinned {
        &self.pinned
    }

    /// Whether targets of this rule are portability or architecture dependent.
    pub fn portability(&self) -> &Portability {
        &self.portability
    }
}
