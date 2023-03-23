use crate::{CacheStatus, Goal, Target};

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum ActionEvent {
    ActionPreparationStarted { target: String, action_count: u64 },
    ActionExecutionStarted { target: String, action: String },
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum ArchiveEvent {
    ArchiveDownloading { target: String, url: String },
    ArchiveUnpacking(String),
    ArchiveVerifying(String),
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum QueueEvent {
    TargetQueued { target_id: String, target: String },
    TargetSkipped { target: String },
    TargetsQueued { target_count: u64 },
    QueueingWorkspace,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum TricorderEvent {
    TricorderServiceStarted { target: String },
    SignatureGenerationStarted { target: String },
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum WorkerEvent {
    TargetBuildStarted {
        target: Target,
        goal: Goal,
    },
    TargetBuildCompleted {
        cache_status: CacheStatus,
        target: Target,
        goal: Goal,
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
    use crate::events::EventChannel;

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
