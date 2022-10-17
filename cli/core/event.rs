use super::*;
use std::path::PathBuf;
use thiserror::*;

#[derive(Error, Debug)]
pub enum BuildError {
    #[error(transparent)]
    BuildWorkerError(BuildWorkerError),

    #[error(transparent)]
    BuildResultError(BuildResultError),

    #[error(transparent)]
    TargetExecutorError(TargetExecutorError),

    #[error(transparent)]
    TargetPlannerError(TargetPlannerError),

    #[error(transparent)]
    LabelResolverError(LabelResolverError),
}

#[derive(Debug)]
pub enum Event {
    ActionRunning { label: Label, action: Action },
    ArchiveDownloading { label: Label, url: String },
    ArchiveUnpacking(Label),
    ArchiveVerifying(Label),
    ResolvingDependency { label: Label },
    BadBuildfile(PathBuf, BuildfileError),
    BuildCompleted(std::time::Instant),
    BuildError(Label, BuildError),
    BuildStarted(std::time::Instant),
    BuildingTarget { label: Label, rule_mnemonic: String },
    CacheHit(Label),
    ErrorLoadingRule(String, BuildWorkerError),
    PreparingActions { label: Label, action_count: u64 },
    QueuedTargets(u64),
    QueueingWorkspace,
    TargetBuilt(Label),
    WorkerError(BuildWorkerError),
    EmptyWorkspace(std::time::Instant),
}
