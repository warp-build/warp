use super::*;
use std::path::PathBuf;

#[derive(Debug)]
pub enum Event {
    BuildCompleted,
    QueuedTarget(Label),
    QueuedTargets(usize),
    RequeueingTarget(Label, Vec<Label>),
    BuildingTarget { label: Label, rule_mnemonic: String },
    ArchiveDownloading { label: Label, url: String },
    ArchiveVerifying(Label),
    ArchiveUnpacking(Label),
    ActionRunning { label: Label, action: Action },
    TargetBuilt(Label),
    BuildError(Label, WorkerError),
    CacheHit(Label, PathBuf),
    CacheMiss{label: Label, local_path: PathBuf},
}
