use super::*;
use crate::events::{Event, EventChannel};
use crate::resolver::*;
use dashmap::{DashMap, DashSet};
use fxhash::FxHashSet;
use std::sync::{Arc, Mutex, RwLock};
use thiserror::*;
use tracing::*;

#[derive(Error, Debug)]
pub enum TaskQueueError {
    #[error("Cannot queue the @all target")]
    CannotQueueTargetAll,

    #[error(transparent)]
    DependencyCycle(TaskResultError),
    /*
    #[error(transparent)]
    WorkspaceScannerError(WorkspaceScannerError),

    #[error(transparent)]
    FileScannerError(FileScannerError),

    #[error(transparent)]
    BuildfileError(anyhow::Error),
    */
}

/// A thread-safe queue for compilation targets, to be consumed by workers.
///
#[derive(Debug)]
pub struct TaskQueue {
    /// Targets currently being built.
    busy_targets: Arc<DashSet<TargetId>>,

    /// Targets currently being built.
    in_queue_targets: Arc<DashSet<TargetId>>,

    /// The queue from which workers pull work.
    inner_queue: Arc<crossbeam::deque::Injector<Task>>,

    /// A backup queue used for set-aside targets.
    wait_queue: Arc<RwLock<FxHashSet<Task>>>,

    /// The list of dependencies needed for a target to be pushed out of the wait queue.
    wait_queue_deps: Arc<DashMap<Task, DashSet<TargetId>>>,

    /// Targets already built.
    task_results: Arc<TaskResults>,

    /// The targets that have been successfully queued.
    all_queued_targets: Arc<DashSet<TargetId>>,

    target_registry: Arc<TargetRegistry>,

    event_channel: Arc<EventChannel>,

    // NOTE(@ostera): only used to serialize the calls to `next` and prevent fetching the same
    // target twice.
    _queue_lock: Arc<Mutex<()>>,
}

impl TaskQueue {
    #[tracing::instrument(name = "TaskQueue::new", skip(target_registry))]
    pub fn new(
        task_results: Arc<TaskResults>,
        target_registry: Arc<TargetRegistry>,
        event_channel: Arc<EventChannel>,
    ) -> TaskQueue {
        TaskQueue {
            busy_targets: Arc::new(DashSet::new()),
            in_queue_targets: Arc::new(DashSet::new()),
            inner_queue: Arc::new(crossbeam::deque::Injector::new()),
            wait_queue: Arc::new(RwLock::new(FxHashSet::default())),
            wait_queue_deps: Arc::new(DashMap::new()),
            task_results,
            all_queued_targets: Arc::new(DashSet::default()),
            target_registry,
            _queue_lock: Arc::new(Mutex::new(())),
            event_channel,
        }
    }

    #[tracing::instrument(name = "TaskQueue::next", skip(self))]
    pub fn next(&self) -> Option<Task> {
        let _lock = self._queue_lock.lock().unwrap();
        loop {
            let task = if let crossbeam::deque::Steal::Success(task) = self.inner_queue.steal() {
                task
            } else if let Ok(mut wait_queue) = self.wait_queue.write() {
                debug!("Inspecting wait_queue");
                let mut task: Option<Task> = None;

                'wait_queue: for t in &*wait_queue {
                    debug!(
                        "Found task {:?}",
                        self.target_registry.get_target(t.target).to_string()
                    );
                    if let Some(deps) = self.wait_queue_deps.get(t) {
                        for dep in (*deps).iter() {
                            debug!("-> {:?}", self.target_registry.get_target(*dep).to_string());
                            if !self.task_results.is_target_built(*dep) {
                                debug!("not built! skipping");
                                continue 'wait_queue;
                            }
                        }
                        task = Some(*t);
                    } else {
                        task = Some(*t);
                    }
                }

                let task = task?;
                debug!(
                    "pulling {}",
                    self.target_registry.get_target(task.target).to_string()
                );
                wait_queue.remove(&task);
                task
            } else {
                return None;
            };

            // If the target is already computed or being computed, we can skip it
            // and try to fetch the next one immediately.
            //
            // When the queue empties up, this will return a None, but otherwise
            // we'll go through a bunch of duplicates, discarding them.
            if self.busy_targets.contains(&task.target)
                || self.task_results.is_target_built(task.target)
            {
                continue;
            }

            // But if it is yet to be built, we mark it as busy
            self.busy_targets.insert(task.target);
            self.in_queue_targets.remove(&task.target);
            return Some(task);
        }
    }

    #[tracing::instrument(name = "TaskQueue::ack", skip(self))]
    pub fn ack(&self, task: Task) {
        self.busy_targets.remove(&task.target);
    }

    #[tracing::instrument(name = "TaskQueue::nack", skip(self))]
    pub fn nack(&self, task: Task) {
        self.busy_targets.remove(&task.target);
        self.wait_queue.write().unwrap().insert(task);
    }

    #[tracing::instrument(name = "TaskQueue::nack", skip(self))]
    pub fn skip(&self, task: Task) {
        self.task_results.remove_expected_target(task.target);
        self.busy_targets.remove(&task.target);
    }

    #[tracing::instrument(name = "TaskQueue::is_target_busy", skip(self))]
    pub fn is_target_busy(&self, target: TargetId) -> bool {
        self.busy_targets.contains(&target)
    }

    #[tracing::instrument(name = "TaskQueue::is_queue_empty", skip(self))]
    pub fn is_empty(&self) -> bool {
        self.inner_queue.is_empty()
    }

    #[tracing::instrument(name = "TaskQueue::queue", skip(self))]
    pub fn queue(&self, task: Task) -> Result<(), TaskQueueError> {
        let target = task.target;
        if self.target_registry.get_target(target).is_all() {
            return Err(TaskQueueError::CannotQueueTargetAll);
        }
        if self.task_results.is_target_built(target)
            || self.busy_targets.contains(&target)
            || self.in_queue_targets.contains(&target)
        {
            self.event_channel.send(Event::QueuedSkipTarget {
                target: self.target_registry.get_target(target).to_string(),
            });
            return Ok(());
        }
        self.task_results.add_expected_target(target);
        self.in_queue_targets.insert(target);
        self.inner_queue.push(task);
        self.all_queued_targets.insert(target);
        self.event_channel.send(Event::QueuedTarget {
            target_id: target.to_string(),
            target: self.target_registry.get_target(target).to_string(),
        });
        Ok(())
    }

    pub fn queue_deps(&self, task: Task, deps: &[TargetId]) -> Result<(), TaskQueueError> {
        self.task_results
            .add_dependencies(task.target, deps)
            .map_err(TaskQueueError::DependencyCycle)?;

        self.wait_queue_deps
            .insert(task, deps.to_vec().into_iter().collect());

        for dep in deps {
            self.queue(Task::build(*dep))?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn queue_emptiness() {}

    #[test]
    fn contiguous_duplicates_are_discarded() {}

    #[tokio::test]
    async fn already_built_targets_are_ignored_when_queueing_new_targets() {}

    #[tokio::test]
    async fn once_a_target_is_built_we_discard_it() {}

    #[test]
    fn nexting_a_target_requires_a_nack_to_get_it_again() {}
}
