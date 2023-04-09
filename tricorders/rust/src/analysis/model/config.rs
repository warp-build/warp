use fxhash::{FxHashMap, FxHashSet};
use std::path::PathBuf;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    String,
    File,
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

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum Value {
    String(String),
    File(PathBuf),
    Target(String),
    List(Vec<Value>),
    // Map(FxHashMap<String, Value>),
}

impl From<&str> for Value {
    fn from(value: &str) -> Self {
        Self::String(value.to_string())
    }
}

#[derive(Debug, Clone, Default, PartialEq)]
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

#[derive(Debug, Clone, Default)]
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
