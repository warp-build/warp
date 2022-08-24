use super::*;

/// A Target in the Warp dependency graph is a labeled instantiation of a rule plus a configuration
/// object.
///
#[derive(Debug, Clone)]
pub struct Target {
    /// The name of this target.
    label: Label,

    /// The dependencies of this target.
    deps: Vec<Label>,

    /// The rule used to build this target.
    rule: Rule,

    /// The target's configuration. To be type-checked against the rule.
    cfg: RuleConfig,

    /// Whether this is a runnable or not
    kind: TargetKind,
}

#[derive(Debug, Copy, PartialEq, Eq, Clone)]
pub enum TargetKind {
    Runnable,
    Buildable,
}

impl Target {
    pub fn new(label: Label, rule: &Rule, cfg: RuleConfig) -> Target {
        let mut deps: Vec<Label> = cfg
            .get_label_list("deps")
            .unwrap_or_default()
            .iter()
            .map(|dep| dep.canonicalize(&label.path()))
            .collect();
        deps.extend_from_slice(rule.toolchains());

        let kind = if rule.runnable == Runnable::Runnable {
            TargetKind::Runnable
        } else {
            TargetKind::Buildable
        };

        Target {
            deps,
            cfg,
            label,
            rule: rule.clone(),
            kind,
        }
    }

    pub fn is_portable(&self) -> bool {
        self.rule.portability == Portability::Portable
    }

    pub fn is_pinned(&self) -> bool {
        self.rule.pinned == Pinned::Pinned
    }

    pub fn kind(&self) -> &TargetKind {
        &self.kind
    }

    pub fn config(&self) -> &RuleConfig {
        &self.cfg
    }

    pub fn rule(&self) -> &Rule {
        &self.rule
    }

    pub fn label(&self) -> &Label {
        &self.label
    }

    pub fn deps(&self) -> &[Label] {
        &self.deps
    }
}

impl std::fmt::Display for Target {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        write!(
            fmt,
            "{}(name = \"{}\")",
            self.rule().mnemonic(),
            self.label().to_string()
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn uses_rule_mnemonic_to_print_itself() {
        let label = Label::new(":test_target");
        let rule = Rule::new(
            "test_rule".to_string(),
            "TestRule".to_string(),
            vec![],
            ConfigSpec::default(),
            RuleConfig::default(),
            Runnable::NotRunnable,
            Pinned::Pinned,
            SandboxConfig::default(),
        );
        let cfg = RuleConfig::default();
        let target = Target::new(label, &rule, cfg);
        assert_eq!("TestRule(name = \":test_target\")", format!("{}", target));
    }

    #[test]
    fn includes_rule_toolchains_in_dependencies() {
        let label = Label::new("test_target");
        let rule = Rule::new(
            "test_rule".to_string(),
            "TestRule".to_string(),
            vec![Label::new("dep")],
            ConfigSpec::default(),
            RuleConfig::default(),
            Runnable::NotRunnable,
            Pinned::Pinned,
            SandboxConfig::default(),
        );
        let cfg = RuleConfig::default();
        let target = Target::new(label, &rule, cfg);
        assert_eq!(1, target.deps().len());
        assert_eq!(Label::new("dep"), target.deps()[0]);
    }
}
