use super::*;
use crate::events::event::QueueEvent;
use crate::events::EventChannel;
use crate::model::TaskId;
use crate::resolver::{SignatureRegistry, TargetRegistry};
use crate::sync::{Arc, Mutex, RwLock};
use crate::Config;
use dashmap::{DashMap, DashSet};
use fxhash::FxHashSet;
use thiserror::*;
use tracing::{instrument, *};

#[derive(Error, Debug)]
pub enum TaskQueueError {
    #[error("Cannot queue the @all target")]
    CannotQueueTaskAll,

    #[error(transparent)]
    DependencyCycle(TaskResultError),
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
    /// Tasks currently being built.
    busy_tasks: Arc<DashSet<TaskId>>,

    /// Tasks currently being built.
    in_queue_tasks: Arc<DashSet<TaskId>>,

    /// The queue from which workers pull work.
    inner_queue: Arc<crossbeam::deque::Injector<TaskId>>,

    /// A backup queue used for set-aside targets.
    wait_queue: Arc<RwLock<FxHashSet<TaskId>>>,

    /// The list of dependencies needed for a target to be pushed out of the wait queue.
    wait_queue_deps: Arc<DashMap<TaskId, DashSet<TaskId>>>,

    /// Tasks already built.
    task_results: Arc<TaskResults>,

    /// Tasks already built.
    task_registry: Arc<TaskRegistry>,

    /// The targets that have been successfully queued.
    all_queued_tasks: Arc<DashSet<TaskId>>,

    target_registry: Arc<TargetRegistry>,
    signature_registry: Arc<SignatureRegistry>,

    event_channel: Arc<EventChannel>,

    // NOTE(@ostera): only used to serialize the calls to `next` and prevent fetching the same
    // target twice.
    _queue_lock: Arc<Mutex<()>>,
}

impl TaskQueue {
    #[instrument(name = "TaskQueue::new", skip(target_registry))]
    pub fn new(
        config: &Config,
        task_results: Arc<TaskResults>,
        task_registry: Arc<TaskRegistry>,
        target_registry: Arc<TargetRegistry>,
        signature_registry: Arc<SignatureRegistry>,
    ) -> TaskQueue {
        TaskQueue {
            busy_tasks: Arc::new(DashSet::new()),
            in_queue_tasks: Arc::new(DashSet::new()),
            inner_queue: Arc::new(crossbeam::deque::Injector::new()),
            wait_queue: Arc::new(RwLock::new(FxHashSet::default())),
            wait_queue_deps: Arc::new(DashMap::new()),
            task_results,
            all_queued_tasks: Arc::new(DashSet::default()),
            task_registry,
            target_registry,
            signature_registry,
            _queue_lock: Arc::new(Mutex::new(())),
            event_channel: config.event_channel(),
        }
    }

    #[instrument(name = "TaskQueue::next", skip(self))]
    pub fn next(&self) -> Option<Task> {
        let _lock = self._queue_lock.lock().unwrap();
        loop {
            let (task_id, task) =
                if let crossbeam::deque::Steal::Success(task_id) = self.inner_queue.steal() {
                    debug!("Stole an id from the queue: {:#?}", task_id);
                    (task_id, self.task_registry.get(task_id))
                } else if let Ok(mut wait_queue) = self.wait_queue.write() {
                    debug!("Inspecting wait_queue");
                    let mut task_id: Option<TaskId> = None;

                    'wait_queue: for t in &*wait_queue {
                        let task = self.task_registry.get(*t);
                        debug!(
                            "Found task {} {} {:?}",
                            self.target_registry
                                .get_target(task.target_id())
                                .to_string(),
                            if let Some(sig_id) = task.signature_id() {
                                self.signature_registry.get(sig_id).name().to_string()
                            } else {
                                "".to_string()
                            },
                            *t
                        );
                        if let Some(deps) = self.wait_queue_deps.get(t) {
                            for dep in (*deps).iter() {
                                let dep = self.task_registry.get(*dep);
                                debug!(
                                    "-> {:?} {:?}",
                                    self.target_registry.get_target(dep.target_id()).to_string(),
                                    dep.id()
                                );
                                if !self.task_results.is_task_completed(dep) {
                                    debug!("missing dependency! skipping");
                                    continue 'wait_queue;
                                }
                            }
                            task_id = Some(*t);
                        } else {
                            task_id = Some(*t);
                        }
                    }

                    let task_id = task_id?;
                    let task = self.task_registry.get(task_id);
                    debug!(
                        "pulling {}",
                        self.target_registry
                            .get_target(task.target_id())
                            .to_string()
                    );
                    wait_queue.remove(&task_id);
                    (task_id, task)
                } else {
                    return None;
                };

            // If the target is already computed or being computed, we can skip it
            // and try to fetch the next one immediately.
            //
            // When the queue empties up, this will return a None, but otherwise
            // we'll go through a bunch of duplicates, discarding them.
            if self.busy_tasks.contains(&task_id) || self.task_results.is_task_completed(task) {
                continue;
            }

            // But if it is yet to be built, we mark it as busy
            self.busy_tasks.insert(task_id);
            self.in_queue_tasks.remove(&task_id);
            return Some(task);
        }
    }

    #[instrument(name = "TaskQueue::ack", skip(self))]
    pub fn ack(&self, task: Task) {
        self.busy_tasks.remove(task.id());
    }

    #[instrument(name = "TaskQueue::nack", skip(self))]
    pub fn nack(&self, task: Task) {
        self.busy_tasks.remove(task.id());
        self.wait_queue.write().unwrap().insert(*task.id());
    }

    #[instrument(name = "TaskQueue::skip", skip(self))]
    pub fn skip(&self, task: Task) {
        self.task_results.remove_expected_task(task);
        self.busy_tasks.remove(task.id());
    }

    #[instrument(name = "TaskQueue::swap", skip(self))]
    pub fn swap(&self, old: Task, new: Task) {
        self.task_registry.update(*old.id(), new);
        self.nack(old);
    }

    #[instrument(name = "TaskQueue::is_target_busy", skip(self))]
    pub fn is_task_busy(&self, task: Task) -> bool {
        self.busy_tasks.contains(task.id())
    }

    #[instrument(name = "TaskQueue::is_queue_empty", skip(self))]
    pub fn is_empty(&self) -> bool {
        self.inner_queue.is_empty()
    }

    #[instrument(name = "TaskQueue::queue", skip(self))]
    pub fn queue(&self, task: Task) -> Result<QueuedTask, TaskQueueError> {
        let target = task.target_id();
        if self.target_registry.get_target(target).is_all() {
            return Err(TaskQueueError::CannotQueueTaskAll);
        }

        if self.task_results.is_task_completed(task)
            || self.busy_tasks.contains(task.id())
            || self.in_queue_tasks.contains(task.id())
        {
            self.event_channel.send(QueueEvent::TaskSkipped {
                target: self.target_registry.get_target(target).to_string(),
                signature: task
                    .signature_id()
                    .map(|sig_id| self.signature_registry.get(sig_id).to_string()),
            });
            return Ok(QueuedTask::Skipped);
        }

        self.task_results.add_expected_task(task);
        self.in_queue_tasks.insert(*task.id());
        self.inner_queue.push(*task.id());
        self.all_queued_tasks.insert(*task.id());

        self.event_channel.send(QueueEvent::TaskQueued {
            target: (*self.target_registry.get_target(target)).clone(),
            signature: task.signature_id().map(|sig_id| {
                let sig = self.signature_registry.get(sig_id);
                sig.name().to_string()
            }),
        });

        Ok(QueuedTask::Queued)
    }

    pub fn queue_deps(&self, task: Task, deps: &[Task]) -> Result<(), TaskQueueError> {
        let dep_ids: Vec<TaskId> = deps.iter().copied().map(|task| *task.id()).collect();

        self.task_results
            .add_dependencies(*task.id(), &dep_ids)
            .map_err(TaskQueueError::DependencyCycle)?;

        self.wait_queue_deps
            .insert(*task.id(), dep_ids.iter().copied().collect());

        for dep in deps {
            self.queue(*dep)?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::{ConcreteTarget, ExecutableSpec, Goal, Signature, Target, UnregisteredTask};
    use crate::store::ArtifactManifest;

    #[test]
    fn new_queues_are_always_empty() {
        let q = TaskQueue::default();
        assert!(q.next().is_none());
        assert!(q.is_empty());
    }

    #[quickcheck]
    fn queues_with_tasks_are_not_empty(goal: Goal, target: Target) {
        let q = TaskQueue::default();
        let target_id = q.target_registry.register_target(target);
        let unreg_task = UnregisteredTask::builder()
            .goal(goal)
            .target_id(target_id)
            .build()
            .unwrap();
        let task = q.task_registry.register(unreg_task);
        assert!(q.queue(task).is_ok());
        assert!(!q.is_empty());
    }

    #[quickcheck]
    fn consuming_from_the_queue_removes_the_task(goal: Goal, target: Target) {
        let q = TaskQueue::default();
        let target_id = q.target_registry.register_target(target);
        let unreg_task = UnregisteredTask::builder()
            .goal(goal)
            .target_id(target_id)
            .build()
            .unwrap();
        let task = q.task_registry.register(unreg_task);
        assert!(q.queue(task).is_ok());
        assert!(q.next().is_some());
        assert!(q.is_empty());
        assert!(q.next().is_none());
    }

    #[quickcheck]
    fn queue_preserves_task_integrity(goal: Goal, target: Target) {
        let q = TaskQueue::default();
        let target_id = q.target_registry.register_target(target);
        let unreg_task = UnregisteredTask::builder()
            .goal(goal)
            .target_id(target_id)
            .build()
            .unwrap();
        let task = q.task_registry.register(unreg_task);
        assert!(q.queue(task).is_ok());
        assert_eq!(q.next().unwrap(), task);
    }

    #[quickcheck]
    fn queue_preserves_ordering(gts: Vec<(Goal, Target)>) {
        let q = TaskQueue::default();

        let mut tasks = vec![];
        for (goal, target) in &gts {
            let target_id = q.target_registry.register_target(target);
            let unreg_task = UnregisteredTask::builder()
                .goal(*goal)
                .target_id(target_id)
                .build()
                .unwrap();
            let task = q.task_registry.register(unreg_task);
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
        let target_id = q.target_registry.register_target(target);
        let unreg_task = UnregisteredTask::builder()
            .goal(goal)
            .target_id(target_id)
            .build()
            .unwrap();
        let task = q.task_registry.register(unreg_task);
        assert!(q.queue(task).is_ok());
        assert!(q.next().is_some());
    }

    #[quickcheck]
    fn contiguous_duplicates_are_discarded(goal: Goal, target: Target) {
        let q = TaskQueue::default();
        let target_id = q.target_registry.register_target(target);
        let unreg_task = UnregisteredTask::builder()
            .goal(goal)
            .target_id(target_id)
            .build()
            .unwrap();
        let task = q.task_registry.register(unreg_task);

        assert!(q.queue(task).is_ok());
        assert!(q.queue(task).is_ok());
        assert!(q.queue(task).is_ok());

        assert!(q.next().is_some());
        assert!(q.next().is_none());
    }

    #[quickcheck]
    fn nack_returns_task_to_queue(goal: Goal, target: Target) {
        let q = TaskQueue::default();
        let target_id = q.target_registry.register_target(target);
        let unreg_task = UnregisteredTask::builder()
            .goal(goal)
            .target_id(target_id)
            .build()
            .unwrap();
        let task = q.task_registry.register(unreg_task);

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
        let unreg_task = UnregisteredTask::builder()
            .goal(goal)
            .target_id(target_id)
            .build()
            .unwrap();
        let task = q.task_registry.register(unreg_task);

        let manifest = ArtifactManifest::default();
        let target = ConcreteTarget::new(goal, target_id, target.into(), "".into(), "".into());
        let spec = ExecutableSpec::builder()
            .goal(goal)
            .target(target.clone())
            .signature(
                Signature::builder()
                    .name("test_signature")
                    .target(target)
                    .rule("test_rule")
                    .deps([])
                    .runtime_deps([])
                    .build()
                    .unwrap(),
            )
            .exec_env(Default::default())
            .hash_and_build(&q.task_results)
            .unwrap();
        q.task_results.add_task_result(task, spec, manifest);

        assert_matches!(q.queue(task), Ok(QueuedTask::Skipped));
    }

    #[quickcheck]
    fn completed_tasks_are_skipped_when_consuming(goal: Goal, target: Target) {
        let q = TaskQueue::default();
        let target_id = q.target_registry.register_target(&target);
        let unreg_task = UnregisteredTask::builder()
            .goal(goal)
            .target_id(target_id)
            .build()
            .unwrap();
        let task = q.task_registry.register(unreg_task);

        assert_matches!(q.queue(task), Ok(QueuedTask::Queued));

        let manifest = ArtifactManifest::default();
        let target = ConcreteTarget::new(goal, target_id, target.into(), "".into(), "".into());
        let spec = ExecutableSpec::builder()
            .goal(goal)
            .target(target.clone())
            .signature(
                Signature::builder()
                    .name("test_signature")
                    .target(target)
                    .rule("test_rule")
                    .deps([])
                    .runtime_deps([])
                    .build()
                    .unwrap(),
            )
            .exec_env(Default::default())
            .hash_and_build(&q.task_results)
            .unwrap();
        q.task_results.add_task_result(task, spec, manifest);

        assert!(q.next().is_none());
    }

    #[cfg(shuttle)]
    #[test]
    fn conc_no_double_consumption() {
        use crate::model::{Goal, Target};
        use crate::sync::*;
        use quickcheck::*;
        use std::collections::{HashMap, HashSet};

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
                    let unreg_task = UnregisteredTask::builder()
                        .goal(*goal)
                        .target_id(target_id)
                        .build()
                        .unwrap();
                    let task = q.task_registry.register(unreg_task);
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
