use super::*;
use fxhash::*;
use std::path::{Path, PathBuf};
use thiserror::*;

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

#[derive(Error, Debug)]
pub enum RuleConfigError {
    #[error("Could not fetch key {0:?} from rule configuration")]
    MissingKey(String),

    #[error("Expected all values in list to be of type {expected:?}, but found {found:?}")]
    UnexpectedValueTypeInList {
        expected: CfgValueType,
        found: CfgValue,
        key: String,
    },

    #[error("Expected key {key:?} to be of type {expected:?} but found {found:?} instead.")]
    KeyHadWrongType {
        expected: CfgValueType,
        found: CfgValue,
        key: String,
    },

    #[error("Expected TOML used to read this target to be a table, instead we found: {toml:?}")]
    RuleShouldBeTable { toml: toml::Value },
}

#[derive(Debug, Clone, Default)]
pub struct RuleConfig {
    config: FxHashMap<String, CfgValue>,
}

impl RuleConfig {
    pub fn new() -> RuleConfig {
        RuleConfig {
            config: FxHashMap::default(),
        }
    }

    pub fn with_defaults(self, config: FxHashMap<String, CfgValue>) -> RuleConfig {
        RuleConfig { config }
    }

    pub fn as_map(&self) -> &FxHashMap<String, CfgValue> {
        &self.config
    }

    pub fn get(&self, key: &str) -> Option<CfgValue> {
        self.config.get(key).cloned()
    }

    pub fn insert(&mut self, key: String, val: CfgValue) -> &mut RuleConfig {
        self.config.insert(key, val);
        self
    }

    pub fn insert_str(&mut self, key: String, val: &str) -> &mut RuleConfig {
        self.insert(key, CfgValue::String(val.to_string()))
    }

    pub fn insert_path(&mut self, key: String, val: &Path) -> &mut RuleConfig {
        self.insert(key, CfgValue::File(val.to_path_buf()))
    }

    pub fn get_string(&self, key: &str) -> Result<String, RuleConfigError> {
        let entry = self
            .config
            .get(key)
            .ok_or_else(|| RuleConfigError::MissingKey(key.to_string()))?;

        if let CfgValue::String(name) = entry {
            Ok(name.to_string())
        } else {
            Err(RuleConfigError::KeyHadWrongType {
                expected: CfgValueType::String,
                found: entry.clone(),
                key: key.to_string(),
            })
        }
    }

    pub fn get_label_list(&self, key: &str) -> Result<Vec<Label>, RuleConfigError> {
        let entry = self
            .config
            .get(key)
            .ok_or_else(|| RuleConfigError::MissingKey(key.to_string()))?;

        if let CfgValue::List(elements) = entry {
            let mut labels = vec![];
            for el in elements {
                if let CfgValue::Label(l) = el {
                    labels.push(l.clone());
                } else if let CfgValue::String(s) = el {
                    labels.push(Label::new(s));
                } else {
                    return Err(RuleConfigError::UnexpectedValueTypeInList {
                        expected: CfgValueType::Label,
                        found: el.clone(),
                        key: key.to_string(),
                    });
                }
            }
            Ok(labels)
        } else {
            Err(RuleConfigError::KeyHadWrongType {
                expected: CfgValueType::List(Box::new(CfgValueType::Label)),
                found: entry.clone(),
                key: key.to_string(),
            })
        }
    }

    pub fn get_file_lists(&self) -> Result<Vec<PathBuf>, RuleConfigError> {
        let mut files = vec![];
        for (_name, entry) in self.config.iter() {
            if let CfgValue::List(elements) = entry {
                for el in elements {
                    if let CfgValue::File(path) = el {
                        files.push(path.clone());
                    }
                }
            }
            if let CfgValue::File(path) = entry {
                files.push(path.clone());
            }
        }
        Ok(files)
    }

    pub fn get_file_list(&self, key: &str) -> Result<Vec<PathBuf>, RuleConfigError> {
        let entry = self
            .config
            .get(key)
            .ok_or_else(|| RuleConfigError::MissingKey(key.to_string()))?;

        if let CfgValue::List(elements) = entry {
            let mut paths = vec![];
            for el in elements {
                if let CfgValue::File(path) = el {
                    paths.push(path.clone());
                } else {
                    return Err(RuleConfigError::UnexpectedValueTypeInList {
                        expected: CfgValueType::File,
                        found: el.clone(),
                        key: key.to_string(),
                    });
                }
            }
            Ok(paths)
        } else {
            Err(RuleConfigError::KeyHadWrongType {
                expected: CfgValueType::List(Box::new(CfgValueType::File)),
                found: entry.clone(),
                key: key.to_string(),
            })
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

    impl From<CfgValue> for serde_json::Value {
        fn from(val: CfgValue) -> Self {
            match val {
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

    impl From<RuleConfig> for serde_json::Value {
        fn from(val: RuleConfig) -> Self {
            let mut map: serde_json::map::Map<String, serde_json::Value> =
                serde_json::map::Map::new();

            for (key, value) in val.config.iter() {
                map.insert(key.to_string(), value.clone().into());
            }

            serde_json::Value::Object(map)
        }
    }
}

pub mod toml_codecs {
    use std::collections::BTreeMap;

    use super::*;
    use toml;

    impl TryFrom<CfgValue> for toml::Value {
        type Error = RuleConfigError;

        fn try_from(value: CfgValue) -> Result<toml::Value, Self::Error> {
            match value {
                CfgValue::String(s) => Ok(toml::Value::String(s)),
                CfgValue::List(arr) => {
                    let mut elements = vec![];
                    for e in arr {
                        let value = TryFrom::try_from(e.clone())?;
                        elements.extend(match value {
                            toml::Value::Array(subparts) => subparts,
                            el => vec![el],
                        })
                    }
                    Ok(toml::Value::Array(elements))
                }
                CfgValue::Label(l) => Ok(toml::Value::String(l.to_string())),
                CfgValue::File(f) => Ok(toml::Value::String(f.to_str().unwrap().to_string())),
            }
        }
    }

    impl TryFrom<&RuleConfig> for FlexibleRuleConfig {
        type Error = RuleConfigError;

        fn try_from(rule: &RuleConfig) -> Result<FlexibleRuleConfig, Self::Error> {
            let mut map: BTreeMap<String, toml::Value> = BTreeMap::default();

            for (key, value) in &rule.config {
                let value: toml::Value = TryFrom::try_from(value.clone())?;
                map.insert(key.to_string(), value);
            }

            Ok(FlexibleRuleConfig(map))
        }
    }

    impl TryFrom<FlexibleRuleConfig> for RuleConfig {
        type Error = RuleConfigError;

        fn try_from(cfg: FlexibleRuleConfig) -> Result<Self, Self::Error> {
            let mut values = RuleConfig::new();

            for (key, value) in cfg.0 {
                let value: CfgValue = TryFrom::try_from(value.clone())?;
                values.insert(key.to_string(), value);
            }

            Ok(values)
        }
    }

    impl TryFrom<toml::Value> for CfgValue {
        type Error = RuleConfigError;

        fn try_from(value: toml::Value) -> Result<CfgValue, Self::Error> {
            match value {
                toml::Value::String(s) => Ok(CfgValue::String(s)),
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

    impl TryFrom<&toml::Value> for RuleConfig {
        type Error = RuleConfigError;

        fn try_from(toml: &toml::Value) -> Result<Self, Self::Error> {
            let mut values = RuleConfig::default();

            let kvs = toml
                .as_table()
                .ok_or_else(|| RuleConfigError::RuleShouldBeTable { toml: toml.clone() })?;

            for (key, value) in kvs {
                let value: CfgValue = TryFrom::try_from(value.clone())?;
                values.insert(key.to_string(), value);
            }

            Ok(values)
        }
    }
}

mod tests {}
