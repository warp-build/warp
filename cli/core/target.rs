use super::*;
use std::fmt;
use std::fmt::{Display, Formatter};
use thiserror::*;

#[derive(Error, Debug)]
pub enum TargetError {
    #[error("Expected TOML used to read this target to be a table, instead we found: {toml:?}")]
    TargetTomlReprMustBeATable { toml: toml::Value },
}

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

    /// The dependencies of this target.
    pub deps: Vec<Label>,

    /// The rule used to build this target.
    pub rule_name: RuleName,

    /// The target's configuration. To be type-checked against the rule.
    pub config: RuleConfig,
}

impl Target {
    pub fn new(label: Label, rule_name: &str, config: RuleConfig) -> Self {
        let deps = config
            .get_label_list("deps")
            .unwrap_or_default()
            .into_iter()
            .map(|dep| {
                if dep.is_local() {
                    let path = if dep.is_relative() {
                        label.path().join(dep.path())
                    } else {
                        dep.path()
                    };

                    Label::builder()
                        .name(dep.name())
                        .workspace(label.workspace().to_str().unwrap().to_string())
                        .from_path(path)
                        .unwrap()
                } else {
                    dep
                }
            })
            .collect();

        Self {
            build_started_at: chrono::Utc::now(),
            deps,
            config,
            label,
            rule_name: rule_name.to_string(),
        }
    }

    pub fn change_workspace(&self, workspace: &Workspace) -> Self {
        let mut new_self = self.clone();
        new_self.label = self.label.change_workspace(workspace);
        new_self.deps = vec![];

        for dep in &self.deps {
            let dep = if dep.is_relative() {
                Label::builder()
                    .name(dep.name())
                    .with_workspace(workspace)
                    .from_path(dep.path())
                    .unwrap()
            } else {
                dep.change_workspace(workspace)
            };
            new_self.deps.push(dep);
        }

        new_self
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
