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

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Value {
    String(String),
    File(PathBuf),
    Target(Target),
    List(Vec<Value>),
    // Map(FxHashMap<String, Value>),
}

#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
pub struct Spec {
    _inner: FxHashMap<String, Type>,
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

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Config {
    _inner: FxHashMap<String, Value>,
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
        FxHashSet::default()
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
