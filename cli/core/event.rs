use super::*;
use std::path::PathBuf;

#[derive(Debug)]
pub enum Event {
    ActionRunning { label: Label, action: Action },
    ArchiveDownloading { label: Label, url: String },
    ArchiveUnpacking(Label),
    ArchiveVerifying(Label),
    BadBuildfile(PathBuf, anyhow::Error),
    BuildCompleted(std::time::Instant),
    BuildError(Label, WorkerError),
    BuildStarted(std::time::Instant),
    BuildingTarget { label: Label, rule_mnemonic: String },
    CacheHit(Label, PathBuf),
    CacheMiss { label: Label, local_path: PathBuf },
    QueuedTarget(Label),
    QueuedTargets(usize),
    QueueingWorkspace,
    RequeueingTarget(Label, Vec<Label>),
    TargetBuilt(Label),
    WorkerError(WorkerError),
}
