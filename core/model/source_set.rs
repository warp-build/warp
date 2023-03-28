use fxhash::FxHashSet;
use serde::{Deserialize, Serialize};
use std::path::PathBuf;

#[derive(Default, Clone, Debug, Serialize, Deserialize)]
pub struct SourceSet {
    _inner: FxHashSet<PathBuf>,
}

impl SourceSet {
    pub fn new(_inner: FxHashSet<PathBuf>) -> Self {
        let _inner = _inner
            .into_iter()
            .map(|path| {
                if path.starts_with("./") {
                    path.strip_prefix("./").unwrap().to_path_buf()
                } else {
                    path
                }
            })
            .collect();

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
        Self::new(_inner)
    }
}
