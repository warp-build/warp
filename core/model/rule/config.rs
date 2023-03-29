use crate::model::{Target, TargetError};
use fxhash::{FxHashMap, FxHashSet};
use serde::{Deserialize, Serialize};
use std::path::PathBuf;
use thiserror::Error;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Type {
    String,
    File,
    Target,
    List(Box<Type>),
    Map(FxHashMap<String, Type>),
}

impl std::hash::Hash for Type {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        if let Type::Map(kvs) = &self {
            for (k, v) in kvs {
                k.hash(state);
                v.hash(state);
            }
        } else {
            core::mem::discriminant(self).hash(state);
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub enum Value {
    String(String),
    File(PathBuf),
    Target(Target),
    List(Vec<Value>),
    // Map(FxHashMap<String, Value>),
}

impl From<&str> for Value {
    fn from(value: &str) -> Self {
        Self::String(value.to_string())
    }
}

impl ToString for Value {
    fn to_string(&self) -> String {
        match self {
            Value::String(s) => s.to_string(),
            Value::File(f) => f.to_string_lossy().to_string(),
            Value::Target(t) => t.to_string(),
            Value::List(_l) => unimplemented!(),
        }
    }
}

#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
pub struct Spec {
    _inner: FxHashMap<String, Type>,
}

impl std::hash::Hash for Spec {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        for (k, v) in &self._inner {
            k.hash(state);
            v.hash(state);
        }
    }
}

impl Spec {
    pub fn new(_inner: FxHashMap<String, Type>) -> Self {
        Self { _inner }
    }

    pub fn get(&self, k: &str) -> Option<&Type> {
        self._inner.get(k)
    }

    pub fn insert(&mut self, k: String, v: Type) {
        self._inner.insert(k, v);
    }

    pub fn as_map(&self) -> &FxHashMap<String, Type> {
        &self._inner
    }
}

#[derive(Debug, Clone, Default, Serialize, Deserialize, Eq, PartialEq)]
pub struct Config {
    _inner: FxHashMap<String, Value>,
}

impl std::hash::Hash for Config {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        for (k, v) in &self._inner {
            k.hash(state);
            v.hash(state);
        }
    }
}

impl Config {
    pub fn new(_inner: FxHashMap<String, Value>) -> Self {
        Self { _inner }
    }

    pub fn get(&self, k: &str) -> Option<&Value> {
        self._inner.get(k)
    }

    pub fn insert(&mut self, k: String, v: Value) {
        self._inner.insert(k, v);
    }

    pub fn get_file_set(&self) -> FxHashSet<PathBuf> {
        let values: Vec<Value> = self._inner.values().cloned().collect();
        Self::_extract_files(&values)
    }

    fn _extract_files(values: &[Value]) -> FxHashSet<PathBuf> {
        let mut files = FxHashSet::default();
        for val in values {
            match val {
                Value::File(file) => {
                    files.insert(file.clone());
                }
                Value::List(values) => {
                    let inner = Self::_extract_files(values);
                    files.extend(inner);
                }
                _ => (),
            }
        }
        files
    }

    pub fn values(&self) -> &FxHashMap<String, Value> {
        &self._inner
    }
}

#[derive(Error, Debug)]
pub enum ConfigError {
    #[error(transparent)]
    TargetError(TargetError),

    #[error("Expected to find value of type {expected:?} but instead found {actual:?}")]
    TypeMismatch {
        expected: Type,
        actual: serde_json::Value,
    },

    #[error("Value {value:?} is not of a supported type.")]
    UnsupportedValueType { value: serde_json::Value },
}

#[cfg(test)]
mod tests {
    use super::*;
    use quickcheck::Arbitrary;

    impl Arbitrary for Config {
        fn arbitrary(g: &mut quickcheck::Gen) -> Self {
            Self::new(Arbitrary::arbitrary(g))
        }
    }
    impl Arbitrary for Value {
        fn arbitrary(g: &mut quickcheck::Gen) -> Self {
            let string = Self::String(Arbitrary::arbitrary(g));
            let file = Self::File(Arbitrary::arbitrary(g));
            let target = Self::Target(Arbitrary::arbitrary(g));
            let list = Self::List(Arbitrary::arbitrary(g));
            g.choose(&[string, file, target, list]).unwrap().clone()
        }
    }
}
