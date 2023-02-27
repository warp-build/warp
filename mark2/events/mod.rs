//! # Wire-friendly Build Execution Events
//!
//! None of these events include references to existing data, and they are all self-contained with
//! primitives that make them easy to encode in a wire-friendly format like Protobuf.
//!
//! NOTE(@ostera): This module is likely going to become the interface of a form of log-streamer
//! protocol that we can use to receive and print feedback from local and remote builds.
//!
use crate::sync::Arc;
use std::path::PathBuf;

type TargetId = String;
type Target = String;
type Action = String;
type Goal = String;

#[derive(Default, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Event {
    /// The "nothing happened" event.
    #[default]
    Noop,
    ActionRunning {
        target: String,
        action: Action,
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
        error: String,
    },
    BuildCompleted(std::time::Instant),
    BuildError {
        target: Target,
        error: String,
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
    EmptyWorkspace {
        end_time: std::time::Instant,
    },
    ErrorLoadingRule {
        rule_name: String,
        error: String,
    },
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
    QueuedTargets {
        target_count: u64,
    },
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
    WorkerError {
        worker_id: String,
        error: String,
    },
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
    use super::*;

    impl quickcheck::Arbitrary for Event {
        fn arbitrary(g: &mut quickcheck::Gen) -> Self {
            let action_running = Self::ActionRunning {
                target: String::arbitrary(g),
                action: Action::arbitrary(g),
            };

            let analyzing_source = Self::AnalyzingSource {
                target: Target::arbitrary(g),
            };

            let archive_downloading = Self::ArchiveDownloading {
                target: Target::arbitrary(g),
                url: String::arbitrary(g),
            };

            let bad_buildfile = Self::BadBuildfile {
                buildfile: PathBuf::arbitrary(g),
                error: String::arbitrary(g),
            };

            let build_error = Self::BuildError {
                target: Target::arbitrary(g),
                error: String::arbitrary(g),
            };

            let building_target = Self::BuildingTarget {
                target: Target::arbitrary(g),
                goal: Goal::arbitrary(g),
                rule_mnemonic: String::arbitrary(g),
            };

            let cache_hit = Self::CacheHit {
                target: Target::arbitrary(g),
                goal: Goal::arbitrary(g),
            };

            let error_loading_rule = Self::ErrorLoadingRule {
                rule_name: String::arbitrary(g),
                error: String::arbitrary(g),
            };

            let handling_target = Self::HandlingTarget {
                target: Target::arbitrary(g),
                goal: Goal::arbitrary(g),
            };

            let preparing_actions = Self::PreparingActions {
                target: Target::arbitrary(g),
                action_count: u64::arbitrary(g),
            };

            let queued_target = Self::QueuedTarget {
                target_id: TargetId::arbitrary(g),
                target: Target::arbitrary(g),
            };

            let resolving_dependency = Self::ResolvingDependency {
                target: Target::arbitrary(g),
                resolver: Target::arbitrary(g),
            };

            let target_built = Self::TargetBuilt {
                target: Target::arbitrary(g),
                goal: Goal::arbitrary(g),
            };

            let worker_error = Self::WorkerError {
                worker_id: String::arbitrary(g),
                error: String::arbitrary(g),
            };

            let archive_unpacking = Self::ArchiveUnpacking(Target::arbitrary(g));

            let archive_verifying = Self::ArchiveVerifying(Target::arbitrary(g));

            let generating_signature = Self::GeneratingSignature {
                target: Target::arbitrary(g),
            };

            let queued_skip_target = Self::QueuedSkipTarget {
                target: Target::arbitrary(g),
            };

            let queued_targets = Self::QueuedTargets {
                target_count: u64::arbitrary(g),
            };

            let started_service = Self::StartedService {
                target: Target::arbitrary(g),
            };

            g.choose(&[
                Self::Noop,
                action_running,
                analyzing_source,
                archive_downloading,
                archive_unpacking,
                archive_verifying,
                bad_buildfile,
                Self::BuildCompleted(std::time::Instant::now()),
                build_error,
                Self::BuildStarted(std::time::Instant::now()),
                building_target,
                cache_hit,
                Self::EmptyWorkspace {
                    end_time: std::time::Instant::now(),
                },
                error_loading_rule,
                generating_signature,
                handling_target,
                preparing_actions,
                queued_target,
                queued_skip_target,
                queued_targets,
                Self::QueueingWorkspace,
                resolving_dependency,
                started_service,
                target_built,
                worker_error,
            ])
            .unwrap()
            .clone()
        }
    }

    #[test]
    fn new_event_channel_is_empty() {
        let ec = EventChannel::new();
        assert!(ec.is_empty());
    }

    #[test]
    fn new_event_consumer_is_empty_on_empty_channel() {
        let ec = EventChannel::new();
        let c = ec.consumer();
        assert!(c.is_empty());
    }

    #[quickcheck]
    fn new_event_consumer_is_nonempty_on_nonempty_channel(event: Event) {
        let ec = EventChannel::new();
        let c = ec.consumer();
        ec.send(event);
        assert!(!c.is_empty());
    }

    #[quickcheck]
    fn event_consumer_needs_to_fetch_to_get_messages(event: Event) {
        let ec = EventChannel::new();
        let c = ec.consumer();
        ec.send(event);
        assert!(c.pop().is_none());
        c.fetch();
        assert!(c.pop().is_some());
    }

    #[quickcheck]
    fn events_are_received_in_the_order_they_are_sent(events: Vec<Event>) {
        let ec = EventChannel::new();
        let c = ec.consumer();

        // first we send all the events
        for event in &events {
            ec.send(event.clone());
        }

        // then we fetch and receive them
        for event in events {
            c.fetch();
            let received_event = c.pop().unwrap();
            assert_eq!(received_event, event)
        }

        // then the queue should be empty
        assert!(ec.is_empty());
        assert!(c.pop().is_none());
    }

    #[quickcheck]
    fn receive_consumes_a_message(event: Event) {
        let ec = EventChannel::new();
        let c = ec.consumer();

        assert!(c.pop().is_none());
        ec.send(event.clone());
        c.fetch();
        assert_matches!(c.pop(), Some(e) if e == event);
        assert!(c.pop().is_none());
    }
}
