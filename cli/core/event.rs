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
    ActionRunning {
        label: Label,
        action: Action,
    },
    ArchiveDownloading {
        label: Label,
        url: String,
    },
    ArchiveUnpacking(Label),
    ArchiveVerifying(Label),
    BadBuildfile(PathBuf, SignatureError),
    BuildCompleted(std::time::Instant),
    BuildError(Label, BuildError),
    BuildStarted(std::time::Instant),
    BuildingTarget {
        label: Label,
        goal: Goal,
        rule_mnemonic: String,
    },
    CacheHit {
        label: Label,
        goal: Goal,
    },
    EmptyWorkspace(std::time::Instant),
    ErrorLoadingRule(String, BuildWorkerError),
    AnalyzingSource {
        label: Label,
    },
    GeneratingSignature {
        label: Label,
    },
    HashedLabel {
        label: LabelId,
        src_hash: String,
        ast_hash: String,
    },
    PreparingActions {
        label: Label,
        action_count: u64,
    },
    QueuedTargets(u64),
    QueueingWorkspace,
    ResolvingDependency {
        label: Label,
    },
    TargetBuilt(Label, Goal),
    WorkerError(BuildWorkerError),
    StartedService {
        label: Label,
    },
    QueuedLabel {
        label: Label,
    },
    QueuedSkipLabel {
        label: Label,
    },
}
