use fxhash::FxHashSet;
use serde::{Deserialize, Serialize};
use std::path::PathBuf;

#[derive(Default, Clone, Debug, Serialize, Deserialize)]
pub struct SourceSet {
    _inner: FxHashSet<PathBuf>,
}

impl SourceSet {
    pub fn new(_inner: FxHashSet<PathBuf>) -> Self {
        Self { _inner }
    }

    pub fn files(&self) -> &FxHashSet<PathBuf> {
        &self._inner
    }

    pub fn add(&mut self, s: PathBuf) {
        self._inner.insert(s);
    }
}

impl From<FxHashSet<PathBuf>> for SourceSet {
    fn from(_inner: FxHashSet<PathBuf>) -> Self {
        Self { _inner }
    }
}
