use super::*;
use std::fmt;
use std::fmt::{Display, Formatter};
use thiserror::*;

#[derive(Error, Debug)]
pub enum TargetError {}

/// A Target in the Warp dependency graph is a labeled instantiation of a rule plus a configuration
/// object.
///
/// It represents something that we want to have built or executed.
///
#[derive(Debug, Clone)]
pub struct Target {
    pub build_started_at: chrono::DateTime<chrono::Utc>,

    /// The name of this target.
    pub label: Label,

    /// The build-time dependencies of this target.
    pub deps: Vec<Label>,

    /// The runtime dependencies of this target.
    pub runtime_deps: Vec<Label>,

    /// The rule used to build this target.
    pub rule_name: RuleName,

    /// The target's configuration. To be type-checked against the rule.
    pub config: RuleConfig,
}

impl Target {
    pub fn new(label: Label, rule_name: &str, config: RuleConfig) -> Self {
        let deps = config.get_label_list("deps").unwrap_or_default();
        let runtime_deps = config.get_label_list("runtime_deps").unwrap_or_default();

        Self {
            build_started_at: chrono::Utc::now(),
            deps,
            runtime_deps,
            config,
            label,
            rule_name: rule_name.to_string(),
        }
    }

    /*
    #[tracing::instrument(name = "Target::with_associated_url")]
    pub fn with_associated_url(&self, url: &url::Url) -> Self {
        let mut new_self = self.clone();
        new_self.label = self.label.with_associated_url(url.clone());
        new_self.deps = vec![];

        for dep in &self.deps {
            let dep = dep.with_associated_url(url.clone());
            new_self.deps.push(dep);
        }

        new_self
    }
    */
}

impl Display for Target {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        write!(
            fmt,
            "{}(name = \"{}\")",
            self.rule_name,
            self.label.to_string()
        )
    }
}

#[cfg(test)]
mod tests {}
