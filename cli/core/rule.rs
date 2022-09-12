use super::*;
use fxhash::*;
use serde::de;
use serde::Deserialize;
use std::collections::HashMap;

pub type RuleName = String;

#[derive(Debug, Clone, PartialEq)]
pub enum Runnable {
    Runnable,
    NotRunnable,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pinned {
    Pinned,
    Unpinned,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Portability {
    Portable,
    ArchitectureDependent,
}

/// A Rule defines what actions to take to perform some work.
///
#[derive(Debug, Clone)]
pub struct Rule {
    /// The name of this rule.
    ///
    /// Rule names are unique in a workspace.
    ///
    pub name: RuleName,

    /// A pretty name to print while running this rule.
    pub mnemonic: String,

    /// The toolchains this tool depends on.
    pub toolchains: Vec<Label>,

    /// The rule's configuration map.
    ///
    /// These are the things that a user can pass in when configuring their target,
    /// and it will always support at least `name: String`.
    ///
    pub config: ConfigSpec,

    /// A map of default configuration values.
    pub defaults: RuleConfig,

    /// Whether this rule is runnable or not
    pub runnable: Runnable,

    /// Whether targets of this rule as pinned or not.
    pub pinned: Pinned,

    /// Whether targets of this rule are portability or architecture dependent.
    pub portability: Portability,
}

impl Rule {
    pub fn new(
        name: RuleName,
        mnemonic: String,
        toolchains: Vec<Label>,
        config: ConfigSpec,
        defaults: RuleConfig,
        runnable: Runnable,
        pinned: Pinned,
        portability: Portability,
    ) -> Rule {
        Rule {
            name,
            mnemonic,
            toolchains,
            config,
            defaults,
            runnable,
            pinned,
            portability,
        }
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

        let mut cfg = FxHashMap::default();
        for (k, t) in json_cfg.iter() {
            let value_type = match t.as_str().unwrap() {
                    "label" => Ok(CfgValueType::Label),
                    "file" => Ok(CfgValueType::File),
                    "string" => Ok(CfgValueType::String),
                    "list_of_label" => Ok(CfgValueType::List(Box::new(CfgValueType::Label))),
                    "list_of_file" => Ok(CfgValueType::List(Box::new(CfgValueType::File))),
                    "list_of_string" => Ok(CfgValueType::List(Box::new(CfgValueType::String))),
                    _ => Err(de::Error::custom(format!("Unrecognized rule config key type {} -- valid types are  label(), file(), string(), and their array variants", t.to_string()))),
                }?;

            cfg.insert(k.to_string(), value_type);
        }
        let config = ConfigSpec(cfg);

        let mut default_cfg = HashMap::new();
        for (k, v) in rule_spec["defaults"]
            .as_object()
            .ok_or_else(|| de::Error::custom("Expected 'defaults' to be an Object".to_string()))?
            .iter()
        {
            let t = config
                .get(k)
                .ok_or_else(|| de::Error::custom(format!("Could not find type for key {:?}", k)))?;
            let typed_value = (v.clone(), t.clone()).into();
            default_cfg.insert(k.to_string(), typed_value);
        }
        let defaults = RuleConfig::new().with_defaults(default_cfg);

        let toolchains: Vec<Label> = (&rule_spec["toolchains"])
            .as_array()
            .ok_or_else(|| {
                de::Error::custom(format!(
                    "Expected RuleSpec 'toolchains' key to be an Array, instead found: {:?}",
                    &rule_spec["toolchains"]
                ))
            })?
            .iter()
            .map(|t| {
                let string = t.as_str().unwrap();
                Label::new(string)
            })
            .collect();

        let runnable = if rule_spec["runnable"].as_bool().unwrap_or(false) {
            Runnable::Runnable
        } else {
            Runnable::NotRunnable
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

        /*
        let sandbox_config = {
            let mut default = serde_json::Map::new();
            default.insert("mode".to_string(), "link".into());

            let sandbox_obj = rule_spec["sandbox"].as_object().unwrap_or(&default);

            let file_mode = if sandbox_obj["mode"].as_str().unwrap_or("link") == "copy" {
                SandboxFileMode::Copy
            } else {
                SandboxFileMode::Link
            };

            SandboxConfig { file_mode }
        };
        */

        let rule = Rule::new(
            name,
            rule_spec["mnemonic"].as_str().unwrap().to_string(),
            toolchains,
            config,
            defaults,
            runnable,
            pinned,
            portability,
        );

        Ok(rule)
    }
}
