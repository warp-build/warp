use fxhash::FxHashMap;
use std::collections::hash_map::Values;
use std::path::PathBuf;

#[derive(Default, Debug, Clone)]
pub struct ProvidedFiles {
    _inner: FxHashMap<String, PathBuf>,
}

impl ProvidedFiles {
    pub fn new(_inner: FxHashMap<String, PathBuf>) -> Self {
        Self { _inner }
    }

    pub fn files(&self) -> &FxHashMap<String, PathBuf> {
        &self._inner
    }

    pub fn get(&self, k: &str) -> Option<&PathBuf> {
        self._inner.get(k)
    }

    pub fn insert(&mut self, k: String, v: PathBuf) {
        self._inner.insert(k, v);
    }

    pub fn values(&self) -> Values<'_, String, PathBuf> {
        self._inner.values()
    }
}
