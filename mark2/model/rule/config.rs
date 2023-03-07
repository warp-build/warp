use std::path::PathBuf;

use crate::model::Target;
use fxhash::{FxHashMap, FxHashSet};
use serde::{Deserialize, Serialize};

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
    pub fn as_map(&self) -> &FxHashMap<String, Type> {
        &self._inner
    }
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Config {
    _inner: FxHashMap<String, Value>,
}

impl Config {
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
