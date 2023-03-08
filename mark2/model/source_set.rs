use fxhash::FxHashSet;
use std::path::PathBuf;

#[derive(Default, Clone, Debug)]
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
