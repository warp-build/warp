use fxhash::FxHashSet;
use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};

#[derive(Clone, Debug, Eq, PartialEq, Hash, Ord, PartialOrd, Serialize, Deserialize)]
pub enum SourceKind {
    File(PathBuf),
    Chunk(PathBuf, String),
}

impl SourceKind {
    pub fn path(&self) -> &Path {
        match self {
            SourceKind::File(path) => path,
            SourceKind::Chunk(path, _) => path,
        }
    }
}

#[derive(Default, Clone, Debug, Serialize, Deserialize)]
pub struct SourceSet {
    _inner: FxHashSet<SourceKind>,
}

impl SourceSet {
    pub fn new(_inner: FxHashSet<SourceKind>) -> Self {
        let _inner = _inner
            .into_iter()
            .map(|src| match src {
                SourceKind::File(path) if path.starts_with("./") => {
                    SourceKind::File(path.strip_prefix("./").unwrap().to_path_buf())
                }
                SourceKind::Chunk(path, chunk) if path.starts_with("./") => {
                    SourceKind::Chunk(path.strip_prefix("./").unwrap().to_path_buf(), chunk)
                }
                _ => src,
            })
            .collect();

        Self { _inner }
    }

    pub fn files(&self) -> Vec<PathBuf> {
        self._inner
            .iter()
            .map(|src| src.path().to_path_buf())
            .collect()
    }

    pub fn add(&mut self, s: SourceKind) {
        self._inner.insert(s);
    }

    pub fn sources(&self) -> &FxHashSet<SourceKind> {
        &self._inner
    }
}

impl From<FxHashSet<SourceKind>> for SourceSet {
    fn from(_inner: FxHashSet<SourceKind>) -> Self {
        Self::new(_inner)
    }
}
