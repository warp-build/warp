// use crate::actions::*;
use crate::planner::*;
use crate::resolver::*;
use crate::worker::*;
use std::path::PathBuf;
use std::sync::Arc;
use thiserror::*;

#[derive(Error, Debug)]
pub enum BuildError {
    #[error(transparent)]
    WorkerError(WorkerError),

    #[error(transparent)]
    TaskResultError(TaskResultError),
    // #[error(transparent)]
    // TargetExecutorError(TargetExecutorError),

    // #[error(transparent)]
    // TargetPlannerError(TargetPlannerError),

    // #[error(transparent)]
    // TargetResolverError(TargetResolverError),
}

#[derive(Debug)]
pub enum Event {
    ActionRunning {
        target: Target,
        // action: Action,
    },
    AnalyzingSource {
        target: Target,
    },
    ArchiveDownloading {
        target: Target,
        url: String,
    },
    ArchiveUnpacking(Target),
    ArchiveVerifying(Target),
    BadBuildfile {
        buildfile: PathBuf,
        error: SignatureError,
    },
    BuildCompleted(std::time::Instant),
    BuildError {
        target: Target,
        error: BuildError,
    },
    BuildStarted(std::time::Instant),
    BuildingTarget {
        target: Target,
        goal: Goal,
        rule_mnemonic: String,
    },
    CacheHit {
        target: Target,
        goal: Goal,
    },
    EmptyWorkspace(std::time::Instant),
    // ErrorLoadingRule(String, WorkerError),
    GeneratingSignature {
        target: Target,
    },
    HandlingTarget {
        target: Target,
        goal: Goal,
    },
    HashedTarget {
        target: TargetId,
        src_hash: String,
        ast_hash: String,
    },
    PreparingActions {
        target: Target,
        action_count: u64,
    },
    QueuedTarget {
        target_id: TargetId,
        target: Target,
    },
    QueuedSkipTarget {
        target: Target,
    },
    QueuedTargets(u64),
    QueueingWorkspace,
    ResolvingDependency {
        target: Target,
        resolver: Target,
    },
    StartedService {
        target: Target,
    },
    TargetBuilt {
        target: Target,
        goal: Goal,
    },
    WorkerError(WorkerError),
}

pub struct EventConsumer {
    channel: Arc<crossbeam::deque::Injector<Event>>,
    queue: crossbeam::deque::Worker<Event>,
}

impl EventConsumer {
    pub fn fetch(&self) {
        let _steal = self.channel.steal_batch(&self.queue);
    }

    pub fn pop(&self) -> Option<Event> {
        self.queue.pop()
    }

    pub fn is_empty(&self) -> bool {
        let _steal = self.channel.steal_batch(&self.queue);
        self.queue.is_empty()
    }
}

impl Iterator for &EventConsumer {
    type Item = Event;

    fn next(&mut self) -> Option<Self::Item> {
        self.pop()
    }
}

#[derive(Clone, Debug, Default)]
pub struct EventChannel {
    bus: Arc<crossbeam::deque::Injector<Event>>,
}

impl EventChannel {
    pub fn new() -> EventChannel {
        EventChannel::default()
    }

    pub fn is_empty(&self) -> bool {
        self.bus.is_empty()
    }

    pub fn send(&self, event: Event) {
        self.bus.push(event)
    }

    pub fn consumer(&self) -> EventConsumer {
        EventConsumer {
            channel: self.bus.clone(),
            queue: crossbeam::deque::Worker::new_fifo(),
        }
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn can_only_receive_after_send() {}

    #[test]
    fn receive_consumes_a_message() {}
}
