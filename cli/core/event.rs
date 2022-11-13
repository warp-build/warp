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
    AnalyzingSource {
        label: Label,
    },
    ArchiveDownloading {
        label: Label,
        url: String,
    },
    ArchiveUnpacking(Label),
    ArchiveVerifying(Label),
    BadBuildfile {
        buildfile: PathBuf,
        error: SignatureError,
    },
    BuildCompleted(std::time::Instant),
    BuildError {
        label: Label,
        error: BuildError,
    },
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
    GeneratingSignature {
        label: Label,
    },
    HandlingTarget {
        label: Label,
        goal: Goal,
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
    QueuedLabel {
        label: Label,
    },
    QueuedSkipLabel {
        label: Label,
    },
    QueuedTargets(u64),
    QueueingWorkspace,
    ResolvingDependency {
        label: Label,
        resolver: Label,
    },
    StartedService {
        label: Label,
    },
    TargetBuilt {
        label: Label,
        goal: Goal,
    },
    WorkerError(BuildWorkerError),
}
