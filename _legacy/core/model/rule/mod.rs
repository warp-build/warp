mod config;
pub mod expander;
pub mod typer;

pub use config::*;

use self::typer::Typer;
use crate::Target;
use serde::{de, Deserialize, Serialize};

pub type RuleName = String;

#[derive(Default, Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
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

#[derive(Default, Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Pinned {
    #[default]
    Pinned,
    Unpinned,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Portability {
    Portable,
    #[default]
    ArchitectureDependent,
}

/// A Rule defines what actions to take to perform some work.
///
#[derive(Debug, Clone, Default, Serialize)]
pub struct Rule {
    name: RuleName,
    mnemonic: String,
    toolchains: Vec<Target>,
    spec: Spec,
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
    pub fn spec(&self) -> &Spec {
        &self.spec
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

impl<'de> Deserialize<'de> for Rule {
    fn deserialize<D>(de: D) -> Result<Rule, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let json: serde_json::Value = serde::Deserialize::deserialize(de)?;

        let rule_spec = json.as_object().ok_or_else(|| {
            de::Error::custom(format!(
                "Expeced RuleSpec to be an Object, instead found: {:?}",
                json
            ))
        })?;

        let name = rule_spec["name"].as_str().unwrap().to_string();

        let json_cfg = &rule_spec["cfg"].as_object().ok_or_else(|| {
            de::Error::custom(format!(
                "Expected RuleSpec 'cfg' key to be an Object, instead found: {:?}",
                &rule_spec["cfg"]
            ))
        })?;

        // NOTE(@ostera): Parse the Specification of the Rule Config into a rule::Spec object. We
        // will use this later on to validate the defaults and the overrides.
        let mut spec = Spec::default();
        for (k, t) in json_cfg.iter() {
            let value_type = match t.as_str().unwrap() {
                    "target" => Ok(Type::Target),
                    "file" => Ok(Type::File),
                    "string" => Ok(Type::String),
                    "list_of_target" => Ok(Type::List(Box::new(Type::Target))),
                    "list_of_file" => Ok(Type::List(Box::new(Type::File))),
                    "list_of_string" => Ok(Type::List(Box::new(Type::String))),
                    _ => Err(de::Error::custom(format!("Unrecognized rule config key type {} -- valid types are  target(), file(), string(), and their array variants", t))),
                }?;

            spec.insert(k.to_string(), value_type);
        }

        // NOTE(@ostera): Extract the default values for the rule config, and type-check them.
        let mut defaults = Config::default();
        for (k, v) in rule_spec["defaults"]
            .as_object()
            .ok_or_else(|| de::Error::custom("Expected 'defaults' to be an Object".to_string()))?
            .iter()
        {
            let t = spec
                .get(k)
                .ok_or_else(|| de::Error::custom(format!("Could not find type for key {:?}", k)))?;
            let typed_value = Typer::typecheck(v.clone(), t.clone()).unwrap();
            defaults.insert(k.to_string(), typed_value);
        }

        let toolchains: Vec<Target> = rule_spec["toolchains"]
            .as_array()
            .ok_or_else(|| {
                de::Error::custom(format!(
                    "Expected RuleSpec 'toolchains' key to be an Array, instead found: {:?}",
                    &rule_spec["toolchains"]
                ))
            })?
            .iter()
            .map(|t| t.as_str().unwrap().parse().unwrap())
            .collect();

        let kind = match rule_spec["kind"].as_str().unwrap_or("build") {
            "test" => RuleKind::Test,
            "run" => RuleKind::Run,
            _ => RuleKind::Build,
        };

        let pinned = if rule_spec["pinned"].as_bool().unwrap_or(false) {
            Pinned::Pinned
        } else {
            Pinned::Unpinned
        };

        let portability = if rule_spec["portable"].as_bool().unwrap_or(false) {
            Portability::Portable
        } else {
            Portability::ArchitectureDependent
        };

        let mnemonic = rule_spec["mnemonic"].as_str().unwrap().to_string();

        Ok(Rule {
            name,
            mnemonic,
            toolchains,
            spec,
            defaults,
            kind,
            pinned,
            portability,
        })
    }
}
