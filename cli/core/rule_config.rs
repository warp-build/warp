use super::Label;
use anyhow::*;
use dashmap::DashMap;
use fxhash::*;
use std::path::PathBuf;

#[derive(Debug, Clone, PartialEq)]
pub enum CfgValueType {
    String,
    Label,
    File,
    List(Box<CfgValueType>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum CfgValue {
    String(String),
    Label(Label),
    File(PathBuf),
    List(Vec<CfgValue>),
}

#[derive(Debug, Clone, Default, PartialEq)]
pub struct ConfigSpec(pub FxHashMap<String, CfgValueType>);

impl ConfigSpec {
    pub fn get(&self, key: &str) -> Option<&CfgValueType> {
        self.0.get(key)
    }

    pub fn keys(&self) -> Vec<String> {
        self.0.keys().map(|k| k.to_string()).collect()
    }

    pub fn as_map(&self) -> &FxHashMap<String, CfgValueType> {
        &self.0
    }
}

#[derive(Debug, Clone, Default)]
pub struct RuleConfig {
    pub name: String,
    config: DashMap<String, CfgValue>,
}

impl RuleConfig {
    pub fn new(name: String) -> RuleConfig {
        RuleConfig {
            name,
            config: DashMap::new(),
        }
    }

    pub fn with_defaults(self, config: DashMap<String, CfgValue>) -> RuleConfig {
        RuleConfig { config, .. self }
    }

    pub fn as_map(&self) -> &DashMap<String, CfgValue> {
        &self.config
    }

    pub fn get(&self, key: &str) -> Option<CfgValue> {
        self.config.get(key).map(|entry| entry.value().clone())
    }

    pub fn insert(&self, key: String, val: CfgValue) -> &RuleConfig {
        self.config.insert(key, val);
        self
    }

    pub fn insert_str(&self, key: String, val: &str) -> &RuleConfig {
        self.insert(key, CfgValue::String(val.to_string()))
    }

    pub fn insert_path(&self, key: String, val: &PathBuf) -> &RuleConfig {
        self.insert(key, CfgValue::File(val.clone()))
    }

    pub fn get_label_list(&self, key: &str) -> Result<Vec<Label>, anyhow::Error> {
        let entry = self
            .config
            .get(key)
            .context(format!("Could not find key {:?}", key))?;

        if let CfgValue::List(elements) = entry.value() {
            let mut labels = vec![];
            for el in elements {
                if let CfgValue::Label(l) = el {
                    labels.push(l.clone());
                } else {
                    return Err(anyhow!(
                        "Expected all values in {:?} to be labels, but found {:?}",
                        key,
                        el
                    ));
                }
            }
            Ok(labels)
        } else {
            Err(anyhow!(
                "Expected key {:?} to be a label list but found {:?} instead.",
                key,
                entry.value()
            ))
        }
    }

    pub fn get_file_lists(&self) -> Result<Vec<PathBuf>, anyhow::Error> {
        let mut files = vec![];
        for entry in self.config.iter() {
            if let CfgValue::List(elements) = entry.value() {
                for el in elements {
                    if let CfgValue::File(path) = el {
                        files.push(path.clone());
                    }
                }
            }
            if let CfgValue::File(path) = entry.value() {
                files.push(path.clone());
            }
        }
        Ok(files)
    }

    pub fn get_file_list(&self, key: &str) -> Result<Vec<PathBuf>, anyhow::Error> {
        let entry = self
            .config
            .get(key)
            .context(format!("Could not find key {:?}", key))?;

        if let CfgValue::List(elements) = entry.value() {
            let mut paths = vec![];
            for el in elements {
                if let CfgValue::File(path) = el {
                    paths.push(path.clone());
                } else {
                    return Err(anyhow!(
                        "Expected all values in {:?} to be file paths, but found {:?}",
                        key,
                        el
                    ));
                }
            }
            Ok(paths)
        } else {
            Err(anyhow!(
                "Expected key {:?} to be a file list but found {:?} instead.",
                key,
                entry.value()
            ))
        }
    }
}

pub mod json_codecs {
    use super::*;

    impl From<(serde_json::Value, CfgValueType)> for CfgValue {
        fn from(spec: (serde_json::Value, CfgValueType)) -> CfgValue {
            let (json, type_) = spec;
            match json {
                serde_json::Value::String(string) => match type_ {
                    CfgValueType::String => CfgValue::String(string),
                    CfgValueType::Label => CfgValue::Label(Label::new(&string)),
                    CfgValueType::File => CfgValue::File(PathBuf::from(string)),
                    CfgValueType::List(_) => panic!("Oops!"),
                },
                serde_json::Value::Array(parts) => match type_ {
                    CfgValueType::List(element_type) => CfgValue::List(
                        parts
                            .iter()
                            .map(|e| (e.clone(), *element_type.clone()).into())
                            .collect(),
                    ),
                    _ => panic!("oops!"),
                },
                _ => panic!("oops!"),
            }
        }
    }

    impl Into<serde_json::Value> for CfgValue {
        fn into(self) -> serde_json::Value {
            match self {
                CfgValue::String(string) => serde_json::Value::String(string),
                CfgValue::Label(label) => serde_json::Value::String(label.to_string()),
                CfgValue::File(path) => {
                    serde_json::Value::String(path.to_str().unwrap().to_string())
                }
                CfgValue::List(parts) => {
                    serde_json::Value::Array(parts.iter().map(|e| e.clone().into()).collect())
                }
            }
        }
    }

    impl Into<serde_json::Value> for RuleConfig {
        fn into(self) -> serde_json::Value {
            let mut map: serde_json::map::Map<String, serde_json::Value> =
                serde_json::map::Map::new();

            for entry in self.config.iter() {
                map.insert(entry.key().to_string(), entry.value().clone().into());
            }

            serde_json::Value::Object(map)
        }
    }
}

pub mod toml_codecs {
    use super::*;
    use toml;
    use thiserror::*;

    #[derive(Error, Debug)]
    pub enum ParseError {
        #[error("Something went wrong.")]
        Unknown,
    }

    impl TryFrom<toml::Value> for CfgValue {
        type Error = anyhow::Error;

        fn try_from(value: toml::Value) -> Result<CfgValue, anyhow::Error> {
            match value {
                toml::Value::String(s) => {
                    Ok(CfgValue::String(s.to_string()))
                }
                toml::Value::Array(arr) => {
                    let mut elements = vec![];
                    for e in arr {
                        let value = TryFrom::try_from(e.clone())?;
                        elements.extend(match value {
                            CfgValue::List(subparts) => subparts,
                            el => vec![el],
                        })
                    }
                    Ok(CfgValue::List(elements))
                }
                toml::Value::Integer(val) => panic!("Integer not supported: {:?}", val),
                toml::Value::Float(val) => panic!("Float not supported: {:?}", val),
                toml::Value::Boolean(val) => panic!("Boolean not supported: {:?}", val),
                toml::Value::Datetime(val) => panic!("Datetime not supported: {:?}", val),
                toml::Value::Table(val) => panic!("Table not supported: {:?}", val),
            }
        }
    }

    impl TryFrom<(String, toml::Value)> for RuleConfig {
        type Error = anyhow::Error;

        fn try_from((name, cfg): (String, toml::Value)) -> Result<RuleConfig, anyhow::Error> {
            let table = cfg.as_table().context(format!(
                "Expected a rule configuration to be a TOML Table, but instead found {:?}",
                cfg
            ))?;

            let values = RuleConfig::new(name);

            for (key, value) in table {
                let value: CfgValue = TryFrom::try_from(value.clone())?;
                values.insert(key.to_string(), value);
            }

            Ok(values)
        }
    }

    pub fn parse_rules(toml: &toml::Value) -> Result<Vec<RuleConfig>, anyhow::Error> {
        let mut rules = vec![];

        let toml = toml.as_table().context(format!(
            "Expected rule to be a TOML table but instead found: {:?}",
            &toml
        ))?;
        for (rule_name, rule) in toml.iter() {
            let rule: RuleConfig = (rule_name.clone(), rule.clone()).try_into()?;
            rules.push(rule);
        }

        Ok(rules)
    }
}
