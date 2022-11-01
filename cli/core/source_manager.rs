use super::*;
use dashmap::DashMap;
use sha2::Digest;
use sha2::Sha256;
use std::fmt::Display;
use std::path::Path;
use std::sync::Arc;
use thiserror::*;
use tokio::fs;
use tokio::io::AsyncReadExt;

pub type SourceHash = String;

#[derive(Default, Debug, Clone)]
pub struct SourceHasher {}

pub enum SourceHasherError {}

impl SourceHasher {
    pub async fn hash_source(file: &Path) -> Result<SourceHash, SourceHasherError> {
        let mut f = fs::File::open(&file).await.unwrap();
        let mut buffer = Vec::with_capacity(2048);
        f.read_to_end(&mut buffer).await.unwrap();

        let mut s = Sha256::new();
        s.update(buffer);
        Ok(format!("{:x}", s.finalize()))
    }
}

#[derive(Default, Debug, Clone, Hash, PartialOrd, Ord, PartialEq, Eq)]
pub struct SourceFile {
    contents: Vec<u8>,
}

impl AsRef<[u8]> for SourceFile {
    fn as_ref(&self) -> &[u8] {
        &self.contents
    }
}

#[derive(Default, Debug, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub enum SourceSymbol {
    #[default]
    All,
    Test(String),
}

impl SourceSymbol {
    pub fn from_label_and_goal(label_id: LabelId, label: &Label, goal: Goal) -> Self {
        Self::All
    }
}

/// A unique identifier for a source. It can only be constructed via `LabelRegistry::register`.
///
#[derive(Copy, Default, Debug, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct SourceId(u128);

impl Display for SourceId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(Clone, Debug)]
pub struct SourceManager {
    label_srcs: DashMap<LabelId, SourceId>,
    srcs: DashMap<SourceId, Arc<SourceFile>>,

    build_results: Arc<BuildResults>,
    label_registry: Arc<LabelRegistry>,
    event_channel: Arc<EventChannel>,
    artifact_store: Arc<ArtifactStore>,
}

#[derive(Error, Debug)]
pub enum SourceManagerError {
    #[error("Label does not point to a file")]
    InvalidLabel { label_id: LabelId, label: Label },
}

impl SourceManager {
    pub fn new(
        build_results: Arc<BuildResults>,
        event_channel: Arc<EventChannel>,
        artifact_store: Arc<ArtifactStore>,
        label_registry: Arc<LabelRegistry>,
    ) -> Self {
        Self {
            build_results,
            event_channel,
            artifact_store,
            label_registry,
            label_srcs: Default::default(),
            srcs: Default::default(),
        }
    }

    pub async fn register_source(
        &self,
        label_id: LabelId,
        label: &Label,
    ) -> Result<SourceId, SourceManagerError> {
        Err(SourceManagerError::InvalidLabel {
            label_id,
            label: label.clone(),
        })
    }

    pub fn get(&self, source_id: SourceId) -> Arc<SourceFile> {
        self.srcs.get(&source_id).map(|r| (*r).clone()).unwrap()
    }

    pub async fn get_source_chunk_by_symbol(
        &self,
        source: SourceId,
        symbol: &SourceSymbol,
    ) -> Result<Option<SourceId>, SourceManagerError> {
        Ok(None)
    }
}
