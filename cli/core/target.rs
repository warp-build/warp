use super::*;
use std::fmt;
use std::fmt::{Display, Formatter};

/// A Target in the Warp dependency graph is a labeled instantiation of a rule plus a configuration
/// object.
///
#[derive(Debug, Clone)]
pub struct Target {
    /// The name of this target.
    pub label: Label,

    /// The dependencies of this target.
    pub deps: Vec<Label>,

    /// The rule used to build this target.
    pub rule_name: RuleName,

    /// The target's configuration. To be type-checked against the rule.
    pub config: RuleConfig,
}

impl Target {
    pub fn new(label: Label, rule_name: &str, config: RuleConfig) -> Target {
        let mut deps: Vec<Label> = config
            .get_label_list("deps")
            .unwrap_or_default()
            .iter()
            .map(|dep| dep.canonicalize(&label.path()))
            .collect();

        Target {
            deps,
            config,
            label,
            rule_name: rule_name.to_string(),
        }
    }
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
