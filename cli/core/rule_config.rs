use super::*;
use anyhow::*;
use fxhash::*;
use std::collections::HashMap;
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
    config: HashMap<String, CfgValue>,
    pub name: String,
}

impl RuleConfig {
    pub fn new(name: String) -> RuleConfig {
        RuleConfig {
            name,
            config: HashMap::new(),
        }
    }

    pub fn with_defaults(self, config: HashMap<String, CfgValue>) -> RuleConfig {
        RuleConfig { config, ..self }
    }

    pub fn as_map(&self) -> &HashMap<String, CfgValue> {
        &self.config
    }

    pub fn get(&self, key: &str) -> Option<CfgValue> {
        self.config.get(key).map(|entry| entry.clone())
    }

    pub fn insert(&mut self, key: String, val: CfgValue) -> &mut RuleConfig {
        self.config.insert(key, val);
        self
    }

    pub fn insert_str(&mut self, key: String, val: &str) -> &mut RuleConfig {
        self.insert(key, CfgValue::String(val.to_string()))
    }

    pub fn insert_path(&mut self, key: String, val: &PathBuf) -> &mut RuleConfig {
        self.insert(key, CfgValue::File(val.clone()))
    }

    pub fn get_label_list(&self, key: &str) -> Result<Vec<Label>, anyhow::Error> {
        let entry = self
            .config
            .get(key)
            .context(format!("Could not find key {:?}", key))?;

        if let CfgValue::List(elements) = entry {
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
                entry
            ))
        }
    }

    pub fn get_file_lists(&self) -> Result<Vec<PathBuf>, anyhow::Error> {
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

    pub fn get_file_list(&self, key: &str) -> Result<Vec<PathBuf>, anyhow::Error> {
        let entry = self
            .config
            .get(key)
            .context(format!("Could not find key {:?}", key))?;

        if let CfgValue::List(elements) = entry {
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
                entry
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

            for (key, value) in self.config.iter() {
                map.insert(key.to_string(), value.clone().into());
            }

            serde_json::Value::Object(map)
        }
    }
}

pub mod toml_codecs {
    use std::collections::BTreeMap;

    use super::*;
    use thiserror::*;
    use toml;

    #[derive(Error, Debug)]
    pub enum ParseError {
        #[error("Something went wrong.")]
        Unknown,
    }

    impl TryFrom<CfgValue> for toml::Value {
        type Error = anyhow::Error;

        fn try_from(value: CfgValue) -> Result<toml::Value, anyhow::Error> {
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
        type Error = anyhow::Error;

        fn try_from(rule: &RuleConfig) -> Result<FlexibleRuleConfig, anyhow::Error> {
            let mut map: BTreeMap<String, toml::Value> = BTreeMap::default();

            for (key, value) in &rule.config {
                let value: toml::Value = TryFrom::try_from(value.clone())?;
                map.insert(key.to_string(), value);
            }

            Ok(FlexibleRuleConfig(map))
        }
    }

    impl TryFrom<toml::Value> for CfgValue {
        type Error = anyhow::Error;

        fn try_from(value: toml::Value) -> Result<CfgValue, anyhow::Error> {
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

    impl TryFrom<(String, FlexibleRuleConfig)> for RuleConfig {
        type Error = anyhow::Error;

        fn try_from(
            (name, cfg): (String, FlexibleRuleConfig),
        ) -> Result<RuleConfig, anyhow::Error> {
            let mut values = RuleConfig::new(name);

            for (key, value) in cfg.0 {
                let value: CfgValue = TryFrom::try_from(value.clone())?;
                values.insert(key.to_string(), value);
            }

            Ok(values)
        }
    }
}
