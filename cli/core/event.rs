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
    CacheHit(Label),
    ErrorLoadingRule(String, WorkerError),
    PreparingActions { label: Label, action_count: u64 },
    QueuedTargets(u64),
    QueueingWorkspace,
    TargetBuilt(Label),
    WorkerError(WorkerError),
}
