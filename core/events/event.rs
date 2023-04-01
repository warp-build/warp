use std::path::PathBuf;

use crate::model::SourceKind;
use crate::{CacheStatus, Goal, Target};
use url::Url;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum PackerEvent {
    PackagingStarted { target: Target },
    PackagingCompleted { target: Target },
    UploadStarted { url: Url },
    UploadCompleted { url: Url },
    UploadSkipped { url: Url },
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum ActionEvent {
    ActionPreparationStarted { target: String, action_count: u64 },
    ActionExecutionStarted { target: String, action: String },
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum ArchiveEvent {
    ArchiveDownloading {
        target: String,
        url: String,
    },
    ArchiveUnpacking(String),
    ArchiveVerifying(String),
    CompressionStarted {
        target: String,
        sha256: String,
        total_files: usize,
    },
    CompressionProgress {
        target: String,
        current: usize,
        total_files: usize,
    },
    CompressionCompleted {
        target: String,
        sha256: String,
        total_files: usize,
    },
    CompressionCached {
        target: String,
        sha256: String,
        total_files: usize,
    },
    DownloadProgress {
        url: Url,
        progress: usize,
        total_size: u64,
    },
    DownloadCompleted {
        url: Url,
        sha256: String,
        total_size: u64,
    },
    DownloadStarted {
        url: Url,
    },
    ExtractionStarted {
        source: PathBuf,
        destination: PathBuf,
        url: Url,
    },
    ExtractionCompleted {
        source: PathBuf,
        destination: PathBuf,
        url: Url,
    },
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum QueueEvent {
    TaskQueued {
        target: Target,
        signature: Option<String>,
    },
    TaskSkipped {
        target: String,
        signature: Option<String>,
    },
    TargetsQueued {
        target_count: u64,
    },
    QueueingWorkspace,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum TricorderEvent {
    SignatureGenerationCompleted {
        target: String,
    },
    SignatureGenerationStarted {
        target: String,
    },
    SourceChunkingStarted {
        src: PathBuf,
        sig_name: String,
    },
    SourceChunkingCompleted {
        src: PathBuf,
        sig_name: String,
        source: SourceKind,
    },
    TricorderConnectionEstablished {
        tricorder_url: Url,
    },
    TricorderReadyingCompleted {
        tricorder_url: Url,
    },
    TricorderReadyingStarted {
        tricorder_url: Url,
    },
    TricorderServiceStarted {
        tricorder_url: Url,
    },
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum WorkerEvent {
    TargetBuildStarted {
        goal: Goal,
        target: Target,
    },
    TargetBuildCompleted {
        goal: Goal,
        target: Target,
        signature: String,
        cache_status: CacheStatus,
    },
    BuildError {
        target: String,
        error: String,
    },
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum WorkflowEvent {
    BuildStarted(std::time::Instant),
    BuildCompleted(std::time::Instant),
    Shutdown,
}

impl WorkflowEvent {
    pub(crate) fn build_completed() -> Self {
        Self::BuildCompleted(std::time::Instant::now())
    }
    pub(crate) fn build_started() -> Self {
        Self::BuildStarted(std::time::Instant::now())
    }
}

#[derive(Default, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Event {
    /// The "nothing happened" event.
    #[default]
    Noop,
    ActionEvent(ActionEvent),
    ArchiveEvent(ArchiveEvent),
    QueueEvent(QueueEvent),
    TricorderEvent(TricorderEvent),
    WorkerEvent(WorkerEvent),
    WorkflowEvent(WorkflowEvent),
    PackerEvent(PackerEvent),
}

impl From<PackerEvent> for Event {
    fn from(value: PackerEvent) -> Self {
        Event::PackerEvent(value)
    }
}

impl From<ActionEvent> for Event {
    fn from(value: ActionEvent) -> Self {
        Event::ActionEvent(value)
    }
}

impl From<ArchiveEvent> for Event {
    fn from(value: ArchiveEvent) -> Self {
        Event::ArchiveEvent(value)
    }
}

impl From<QueueEvent> for Event {
    fn from(value: QueueEvent) -> Self {
        Event::QueueEvent(value)
    }
}

impl From<TricorderEvent> for Event {
    fn from(value: TricorderEvent) -> Self {
        Event::TricorderEvent(value)
    }
}

impl From<WorkerEvent> for Event {
    fn from(value: WorkerEvent) -> Self {
        Event::WorkerEvent(value)
    }
}

impl From<WorkflowEvent> for Event {
    fn from(value: WorkflowEvent) -> Self {
        Event::WorkflowEvent(value)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    impl quickcheck::Arbitrary for Event {
        fn arbitrary(g: &mut quickcheck::Gen) -> Self {
            let action_running = ActionEvent::ActionExecutionStarted {
                target: String::arbitrary(g),
                action: String::arbitrary(g),
            };

            g.choose(&[Self::Noop, action_running.into()])
                .unwrap()
                .clone()
        }
    }
}
