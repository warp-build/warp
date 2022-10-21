use super::*;
use std::fmt;
use std::fmt::{Display, Formatter};
use thiserror::*;
use tracing::*;

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
                if dep.is_remote() {
                    return dep;
                }

                let path = if dep.is_relative() {
                    // NOTE(@ostera): if we found a label that is defined in the same buildfile
                    // then its path is the same path as the current label
                    if dep.path().to_str().unwrap().is_empty() {
                        label.path()
                    } else {
                        label.path().join(dep.path())
                    }
                } else {
                    dep.path()
                };

                Label::builder()
                    .name(dep.name())
                    .workspace(label.workspace().to_str().unwrap().to_string())
                    .from_path(path)
                    .unwrap()
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

    #[tracing::instrument(name = "Target::change_workspace", skip(workspace))]
    pub fn change_workspace(&self, workspace: &Workspace) -> Self {
        debug!("changing target workspace: {:?}", &self);
        let mut new_self = self.clone();
        new_self.label = self.label.change_workspace(workspace);
        new_self.deps = vec![];

        for dep in &self.deps {
            let dep = dep.change_workspace(workspace);
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
