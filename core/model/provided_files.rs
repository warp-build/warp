use std::collections::btree_map::Values;
use std::collections::BTreeMap;
use std::path::PathBuf;

#[derive(Default, Debug, Clone)]
pub struct ProvidedFiles {
    _inner: BTreeMap<String, PathBuf>,
}

impl ProvidedFiles {
    pub fn new(_inner: BTreeMap<String, PathBuf>) -> Self {
        Self { _inner }
    }

    pub fn files(&self) -> &BTreeMap<String, PathBuf> {
        &self._inner
    }

    pub fn get(&self, k: &str) -> Option<&PathBuf> {
        self._inner.get(k)
    }

    pub fn insert(&mut self, k: String, v: PathBuf) {
        self._inner.insert(k, v);
    }

    pub fn values(&self) -> Values<String, PathBuf> {
        self._inner.values()
    }
}

impl From<BTreeMap<String, PathBuf>> for ProvidedFiles {
    fn from(_inner: BTreeMap<String, PathBuf>) -> Self {
        Self { _inner }
    }
}
