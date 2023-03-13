use super::*;
use crate::events::{Event, EventChannel};
use crate::model::TargetId;
use crate::resolver::TargetRegistry;
use crate::sync::{Arc, Mutex, RwLock};
use dashmap::{DashMap, DashSet};
use fxhash::FxHashSet;
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

#[derive(Debug, Clone)]
pub enum QueuedTask {
    Skipped,
    Queued,
}

/// A thread-safe queue for compilation targets, to be consumed by workers.
///
#[derive(Default, Debug)]
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
                        self.target_registry.get_target(t.target_id).to_string()
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
                    self.target_registry.get_target(task.target_id).to_string()
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
            if self.busy_targets.contains(&task.target_id)
                || self.task_results.is_target_built(task.target_id)
            {
                continue;
            }

            // But if it is yet to be built, we mark it as busy
            self.busy_targets.insert(task.target_id);
            self.in_queue_targets.remove(&task.target_id);
            return Some(task);
        }
    }

    #[tracing::instrument(name = "TaskQueue::ack", skip(self))]
    pub fn ack(&self, task: Task) {
        self.busy_targets.remove(&task.target_id);
    }

    #[tracing::instrument(name = "TaskQueue::nack", skip(self))]
    pub fn nack(&self, task: Task) {
        self.busy_targets.remove(&task.target_id);
        self.wait_queue.write().unwrap().insert(task);
    }

    #[tracing::instrument(name = "TaskQueue::nack", skip(self))]
    pub fn skip(&self, task: Task) {
        self.task_results.remove_expected_target(task.target_id);
        self.busy_targets.remove(&task.target_id);
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
    pub fn queue(&self, task: Task) -> Result<QueuedTask, TaskQueueError> {
        let target = task.target_id;
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
            return Ok(QueuedTask::Skipped);
        }
        self.task_results.add_expected_target(target);
        self.in_queue_targets.insert(target);
        self.inner_queue.push(task);
        self.all_queued_targets.insert(target);
        self.event_channel.send(Event::QueuedTarget {
            target_id: target.to_string(),
            target: self.target_registry.get_target(target).to_string(),
        });
        Ok(QueuedTask::Queued)
    }

    pub fn queue_deps(&self, task: Task, deps: &[TargetId]) -> Result<(), TaskQueueError> {
        self.task_results
            .add_dependencies(task.target_id, deps)
            .map_err(TaskQueueError::DependencyCycle)?;

        self.wait_queue_deps
            .insert(task, deps.iter().copied().collect());

        for dep in deps {
            self.queue(Task::build(*dep))?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        model::{ConcreteTarget, ExecutableSpec, Goal, Signature, Target},
        store::ArtifactManifest,
    };

    use super::*;

    #[test]
    fn new_queues_are_always_empty() {
        let q = TaskQueue::default();
        assert!(q.next().is_none());
        assert!(q.is_empty());
    }

    #[quickcheck]
    fn queues_with_tasks_are_not_empty(goal: Goal, target: Target) {
        let q = TaskQueue::default();
        let target_id = q.target_registry.register_target(&target);
        let task = Task::new(goal, target_id);
        assert!(q.queue(task).is_ok());
        assert!(!q.is_empty());
    }

    #[quickcheck]
    fn consuming_from_the_queue_removes_the_task(goal: Goal, target: Target) {
        let q = TaskQueue::default();
        let target_id = q.target_registry.register_target(&target);
        let task = Task::new(goal, target_id);
        assert!(q.queue(task).is_ok());
        assert!(q.next().is_some());
        assert!(q.is_empty());
        assert!(q.next().is_none());
    }

    #[quickcheck]
    fn queue_preserves_task_integrity(goal: Goal, target: Target) {
        let q = TaskQueue::default();
        let target_id = q.target_registry.register_target(&target);
        let task = Task::new(goal, target_id);
        assert!(q.queue(task).is_ok());
        assert_eq!(q.next().unwrap(), task);
    }

    #[quickcheck]
    fn queue_preserves_ordering(gts: Vec<(Goal, Target)>) {
        let q = TaskQueue::default();

        let mut tasks = vec![];
        for (goal, target) in &gts {
            let target_id = q.target_registry.register_target(&target);
            let task = Task::new(*goal, target_id);
            if let QueuedTask::Queued = q.queue(task).unwrap() {
                tasks.push(task);
            }
        }

        for task in tasks {
            assert_eq!(q.next().unwrap(), task);
        }

        assert!(q.is_empty());
    }

    #[quickcheck]
    #[should_panic]
    fn queueing_an_unregistered_target_is_an_error(task: Task) {
        let q = TaskQueue::default();
        q.queue(task).unwrap();
    }

    #[quickcheck]
    fn queuing_a_registered_target_makes_the_queue_nonempty(goal: Goal, target: Target) {
        let q = TaskQueue::default();
        let target_id = q.target_registry.register_target(&target);
        let task = Task::new(goal, target_id);
        assert!(q.queue(task).is_ok());
        assert!(q.next().is_some());
    }

    #[quickcheck]
    fn contiguous_duplicates_are_discarded(goal: Goal, target: Target) {
        let q = TaskQueue::default();
        let target_id = q.target_registry.register_target(&target);
        let task = Task::new(goal, target_id);

        assert!(q.queue(task).is_ok());
        assert!(q.queue(task).is_ok());
        assert!(q.queue(task).is_ok());

        assert!(q.next().is_some());
        assert!(q.next().is_none());
    }

    #[quickcheck]
    fn nack_returns_task_to_queue(goal: Goal, target: Target) {
        let q = TaskQueue::default();
        let target_id = q.target_registry.register_target(&target);
        let task = Task::new(goal, target_id);

        assert!(q.queue(task).is_ok());
        let q_task = q.next().unwrap();

        assert!(q.next().is_none());

        q.nack(q_task);

        assert_eq!(q.next().unwrap(), task);
    }

    #[quickcheck]
    fn completed_tasks_are_skipped_when_queueing(goal: Goal, target: Target) {
        let q = TaskQueue::default();
        let target_id = q.target_registry.register_target(&target);
        let task = Task::new(goal, target_id);

        let manifest = ArtifactManifest::default();
        let target = ConcreteTarget::new(Goal::Build, target_id, target.into(), "".into());
        let spec = ExecutableSpec::builder()
            .goal(goal)
            .target(target.clone())
            .signature(
                Signature::builder()
                    .target(target)
                    .rule("test_rule".into())
                    .build()
                    .unwrap(),
            )
            .exec_env(Default::default())
            .hash_and_build(&q.task_results)
            .unwrap();
        q.task_results.add_task_result(target_id, spec, manifest);

        assert_matches!(q.queue(task), Ok(QueuedTask::Skipped));
    }

    #[quickcheck]
    fn completed_tasks_are_skipped_when_consuming(goal: Goal, target: Target) {
        let q = TaskQueue::default();
        let target_id = q.target_registry.register_target(&target);
        let task = Task::new(goal, target_id);

        assert_matches!(q.queue(task), Ok(QueuedTask::Queued));

        let manifest = ArtifactManifest::default();
        let target = ConcreteTarget::new(Goal::Build, target_id, target.into(), "".into());
        let spec = ExecutableSpec::builder()
			.goal(goal)
            .target(target.clone())
            .signature(
                Signature::builder()
                    .target(target)
                    .rule("test_rule".into())
                    .build()
                    .unwrap(),
            )
            .exec_env(Default::default())
            .hash_and_build(&q.task_results)
            .unwrap();
        q.task_results.add_task_result(target_id, spec, manifest);

        assert!(q.next().is_none());
    }

    #[cfg(shuttle)]
    #[test]
    fn conc_no_double_consumption() {
        use crate::model::{Goal, Target};
        use crate::sync::*;
        use quickcheck::*;
        use std::collections::{HashMap, HashSet};
        use std::time::Duration;

        const TASK_COUNT: usize = 50;
        const ITER: usize = 1_000;

        shuttle::check_random(
            move || {
                let mut gen = quickcheck::Gen::new(100);

                // Fill in the queue with arbitrary targets.
                let gts: Vec<(Goal, Target)> = (0..TASK_COUNT)
                    .map(|_| Arbitrary::arbitrary(&mut gen))
                    .collect();
                let q = Arc::new(TaskQueue::default());
                for (goal, target) in &gts {
                    let target_id = q.target_registry.register_target(target);
                    let task = Task::new(*goal, target_id);
                    q.queue(task).unwrap();
                }
                assert!(!q.is_empty());

                // Consume the queue from different threads and collect the results
                let consumed: Arc<RwLock<HashMap<usize, HashSet<Task>>>> =
                    Arc::new(RwLock::new(HashMap::default()));
                let mut handles = vec![];
                for id in 0..4 {
                    let q = q.clone();
                    let consumed = consumed.clone();
                    let handle = thread::spawn(move || {
                        let mut tasks = vec![];
                        while let Some(task) = q.next() {
                            tasks.push(task);
                            q.ack(task);
                        }
                        let tasks: HashSet<Task> = tasks.into_iter().collect();
                        (*consumed.write().unwrap()).insert(id, tasks);
                    });
                    handles.push(handle);
                }

                // Wait for all threads to be finished consuming the queue
                for handle in handles {
                    handle.join().unwrap()
                }
                assert!(consumed.read().unwrap().len() > 0);
                assert!(q.is_empty());

                // Verify all the threads consumed different tasks
                let set_0 = consumed.read().unwrap().get(&0).unwrap().clone();
                let set_1 = consumed.read().unwrap().get(&1).unwrap().clone();
                let set_2 = consumed.read().unwrap().get(&2).unwrap().clone();
                let set_3 = consumed.read().unwrap().get(&3).unwrap().clone();
                assert!(set_0.is_disjoint(&set_1));
                assert!(set_0.is_disjoint(&set_2));
                assert!(set_0.is_disjoint(&set_3));
                assert!(set_1.is_disjoint(&set_0));
                assert!(set_1.is_disjoint(&set_2));
                assert!(set_1.is_disjoint(&set_3));
                assert!(set_2.is_disjoint(&set_0));
                assert!(set_2.is_disjoint(&set_1));
                assert!(set_2.is_disjoint(&set_3));
                assert!(set_3.is_disjoint(&set_0));
                assert!(set_3.is_disjoint(&set_1));
                assert!(set_3.is_disjoint(&set_2));
            },
            ITER,
        );
    }
}
