use super::*;
use fxhash::*;
use serde::{
    de::{self, Visitor},
    ser::{SerializeMap, SerializeSeq},
    Deserialize, Serialize,
};
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

    #[error(transparent)]
    LabelError(LabelError),

    #[error("Expected to find value of type {expected:?} but instead found {actual:?}")]
    TypeMismatch {
        expected: CfgValueType,
        actual: serde_json::Value,
    },

    #[error("Value {value:?} is not of a supported type.")]
    UnsupportedValueType { value: serde_json::Value },
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

    pub fn get_string_list(&self, key: &str) -> Result<Vec<String>, RuleConfigError> {
        let entry = self
            .config
            .get(key)
            .ok_or_else(|| RuleConfigError::MissingKey(key.to_string()))?;

        if let CfgValue::List(elements) = entry {
            let mut labels = vec![];
            for el in elements {
                if let CfgValue::String(l) = el {
                    labels.push(l.clone());
                } else {
                    return Err(RuleConfigError::UnexpectedValueTypeInList {
                        expected: CfgValueType::String,
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

struct RuleConfigVisitor;
impl<'de> Visitor<'de> for RuleConfigVisitor {
    type Value = RuleConfig;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str("a rule configuration should be a JSON object")
    }

    fn visit_map<M>(self, mut access: M) -> Result<Self::Value, M::Error>
    where
        M: serde::de::MapAccess<'de>,
    {
        let mut config = RuleConfig::new();
        while let Some((key, value)) = access.next_entry()? {
            config.insert(key, value);
        }
        Ok(config)
    }
}

impl<'de> Deserialize<'de> for RuleConfig {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_map(RuleConfigVisitor)
    }
}

impl Serialize for RuleConfig {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let rule_map = self.as_map();
        let mut map = serializer.serialize_map(Some(rule_map.len()))?;
        for (k, v) in rule_map {
            map.serialize_entry(k, v)?;
        }
        map.end()
    }
}

struct CfgValueVisitor;
impl<'de> Visitor<'de> for CfgValueVisitor {
    type Value = CfgValue;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str("a rule configuration should be a JSON object")
    }

    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Ok(Self::Value::String(v.to_string()))
    }

    fn visit_string<E>(self, v: String) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Ok(Self::Value::String(v))
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
    where
        A: de::SeqAccess<'de>,
    {
        let mut values = vec![];
        while let Some(value) = seq.next_element()? {
            values.push(value);
        }
        Ok(Self::Value::List(values))
    }
}

impl<'de> Deserialize<'de> for CfgValue {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_any(CfgValueVisitor)
    }
}

impl Serialize for CfgValue {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match self {
            CfgValue::String(s) => serializer.serialize_str(s),
            CfgValue::Label(l) => serializer.serialize_str(&l.to_string()),
            CfgValue::File(p) => serializer.serialize_str(p.to_str().unwrap()),
            CfgValue::List(xs) => {
                let mut seq = serializer.serialize_seq(Some(xs.len()))?;

                for x in xs {
                    seq.serialize_element(x)?;
                }

                seq.end()
            }
        }
    }
}

impl From<proto::google::protobuf::Struct> for RuleConfig {
    fn from(value: proto::google::protobuf::Struct) -> Self {
        let mut config = FxHashMap::default();

        for (key, value) in value.fields {
            config.insert(key.to_string(), value.into());
        }

        RuleConfig { config }
    }
}

impl From<proto::google::protobuf::Value> for CfgValue {
    fn from(value: proto::google::protobuf::Value) -> Self {
        use proto::google::protobuf::*;
        match value.kind.unwrap() {
            value::Kind::StringValue(s) => CfgValue::String(s),
            value::Kind::ListValue(arr) => {
                let mut elements = vec![];
                for e in arr.values {
                    let value = e.into();
                    elements.extend(match value {
                        CfgValue::List(subparts) => subparts,
                        el => vec![el],
                    })
                }
                CfgValue::List(elements)
            }
            value::Kind::NumberValue(val) => panic!("Numbers not supported: {:?}", val),
            value::Kind::BoolValue(val) => panic!("Booleans not supported: {:?}", val),
            value::Kind::StructValue(val) => panic!("Objects not supported: {:?}", val),
            value::Kind::NullValue(_) => panic!("Null not supported"),
        }
    }
}

pub mod json_codecs {
    use std::{collections::BTreeMap, convert::TryFrom};

    use super::*;

    impl TryFrom<serde_json::Value> for RuleConfig {
        type Error = RuleConfigError;

        fn try_from(json: serde_json::Value) -> Result<Self, Self::Error> {
            let mut config = FxHashMap::default();

            for (key, value) in json.as_object().unwrap().iter() {
                config.insert(key.to_string(), value.clone().try_into()?);
            }

            Ok(RuleConfig { config })
        }
    }

    impl TryFrom<BTreeMap<String, serde_json::Value>> for RuleConfig {
        type Error = RuleConfigError;

        fn try_from(json: BTreeMap<String, serde_json::Value>) -> Result<Self, Self::Error> {
            let mut config = FxHashMap::default();

            for (key, value) in json {
                config.insert(key.to_string(), value.clone().try_into()?);
            }

            Ok(RuleConfig { config })
        }
    }

    impl From<RuleConfig> for BTreeMap<String, serde_json::Value> {
        fn from(val: RuleConfig) -> Self {
            let mut map = BTreeMap::new();

            for (key, value) in val.config.iter() {
                map.insert(key.to_string(), value.clone().into());
            }

            map
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

    impl TryFrom<(serde_json::Value, CfgValueType)> for CfgValue {
        type Error = RuleConfigError;

        fn try_from(
            (mut json, type_): (serde_json::Value, CfgValueType),
        ) -> Result<Self, Self::Error> {
            match &mut json {
                serde_json::Value::String(string) => match type_ {
                    CfgValueType::String => Ok(CfgValue::String(std::mem::take(string))),
                    CfgValueType::Label => Ok(CfgValue::Label(
                        string.parse().map_err(RuleConfigError::LabelError)?,
                    )),
                    CfgValueType::File => Ok(CfgValue::File(PathBuf::from(std::mem::take(string)))),
                    CfgValueType::List(_) => Err(RuleConfigError::TypeMismatch {
                        expected: type_.clone(),
                        actual: json.clone(),
                    }),
                },
                serde_json::Value::Array(parts) => match type_ {
                    CfgValueType::List(element_type) => {
                        let mut ps = vec![];
                        for part in parts {
                            let p = (part.clone(), *element_type.clone()).try_into()?;
                            ps.push(p);
                        }
                        Ok(CfgValue::List(ps))
                    }
                    _ => Err(RuleConfigError::TypeMismatch {
                        expected: type_.clone(),
                        actual: json.clone(),
                    }),
                },
                _ => Err(RuleConfigError::UnsupportedValueType {
                    value: json.clone(),
                }),
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

    impl TryFrom<serde_json::Value> for CfgValue {
        type Error = RuleConfigError;

        fn try_from(value: serde_json::Value) -> Result<CfgValue, Self::Error> {
            match value {
                serde_json::Value::String(s) => Ok(CfgValue::String(s)),
                serde_json::Value::Array(arr) => {
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
                serde_json::Value::Number(val) => panic!("Numbers not supported: {:?}", val),
                serde_json::Value::Bool(val) => panic!("Booleans not supported: {:?}", val),
                serde_json::Value::Object(val) => panic!("Objects not supported: {:?}", val),
                serde_json::Value::Null => panic!("Null not supported"),
            }
        }
    }
}

mod tests {}
