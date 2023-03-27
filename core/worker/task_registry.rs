use super::*;
use crate::model::{Task, TaskId, UnregisteredTask};
use crate::sync::{Arc, Mutex};
use dashmap::DashMap;

use tracing::{instrument, *};

/// The Task Registry keeps track of all the tasks being used in the system, all the tasks that
/// have been aliased (due to reparenting, workspace changing, etc).
///
#[derive(Default, Debug, Clone)]
pub struct TaskRegistry {
    ids: DashMap<Task, TaskId>,
    unregistered_tasks: DashMap<UnregisteredTask, TaskId>,
    tasks: DashMap<TaskId, Task>,

    // NOTE(@ostera): only used to serialize the calls to `register` and prevent registering
    // the same task under two different ids.
    _register_lock: Arc<Mutex<()>>,
}

impl TaskRegistry {
    pub fn new() -> Self {
        Self::default()
    }

    /// Get a handle for a Task that can be used to alias and unalias it later.
    ///
    /// If the task has already been registered, the same identifier is returned.
    ///
    #[instrument(name = "TaskRegistry::register", skip(self), ret)]
    pub fn register(&self, unreg_task: UnregisteredTask) -> Task {
        let _lock = self._register_lock.lock().unwrap();
        if let Some(id) = self.find_unregistered(&unreg_task) {
            self.get(id)
        } else {
            let id = TaskId::next();
            let task = Task::builder()
                .id(id)
                .goal(unreg_task.goal())
                .target_id(unreg_task.target_id())
                .signature_id(unreg_task.signature_id())
                .build()
                .unwrap();

            self.ids.insert(task, id);
            self.tasks.insert(id, task);
            self.unregistered_tasks.insert(unreg_task, id);

            task
        }
    }

    /// Get a handle for a collection of targets. Behaves like `TaskRegistry::register_many`.
    ///
    #[instrument(name = "TaskRegistry::register_many", skip(self))]
    pub fn register_many(&self, unreg_tasks: &[UnregisteredTask]) -> Vec<Task> {
        let mut tasks = vec![];
        for task in unreg_tasks {
            tasks.push(self.register(*task));
        }
        tasks
    }

    pub fn update(&self, id: TaskId, mut task: Task) -> Task {
        let _lock = self._register_lock.lock().unwrap();
        if let Some(found_id) = self.find(&task) {
            if id == found_id {
                return task;
            }
        }
        task.set_id(id);
        self.ids.insert(task, id);
        self.tasks.insert(id, task);
        task
    }

    /// Find the id of a task that has already been registered.
    ///
    /// If the Task's corresponding TaskId has been aliased towards some other TaskId, the new
    /// TaskId will be returned.
    ///
    #[instrument(name = "TaskRegistry::find", skip(self))]
    pub fn find(&self, task: &Task) -> Option<TaskId> {
        self.ids.get(task).map(|r| *r.value())
    }

    #[instrument(name = "TaskRegistry::find_unregistered", skip(self))]
    fn find_unregistered(&self, task: &UnregisteredTask) -> Option<TaskId> {
        self.unregistered_tasks.get(task).map(|r| *r.value())
    }

    #[instrument(name = "TaskRegistry::get", skip(self))]
    pub fn get(&self, id: TaskId) -> Task {
        *self.tasks.get(&id).unwrap()
    }

    pub fn len(&self) -> usize {
        self.ids.len()
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use quickcheck::*;

    #[cfg(shuttle)]
    #[test]
    fn conc_registering_the_same_task_returns_the_same_id() {
        use crate::sync::*;

        shuttle::check_dfs(
            move || {
                let mut gen = quickcheck::Gen::new(10);
                let task = UnregisteredTask::arbitrary(&mut gen);
                let reg = Arc::new(TaskRegistry::new());
                let id = reg.register(task.clone());

                let mut handles = vec![];
                for _ in 0..3 {
                    let reg = reg.clone();
                    let task = task.clone();
                    let handle = thread::spawn(move || {
                        reg.register(task.clone());
                    });
                    handles.push(handle);
                }

                for handle in handles {
                    handle.join().unwrap()
                }

                assert_eq!(id, reg.find(&task).unwrap());
                assert_eq!(reg.len(), 1);
            },
            None,
        );
    }

    #[quickcheck]
    fn when_registering_a_task_all_properties_are_preserved(unreg_task: UnregisteredTask) {
        let reg = TaskRegistry::new();
        let task = reg.register(unreg_task);
        assert_eq!(task.goal(), unreg_task.goal());
        assert_eq!(task.target_id(), unreg_task.target_id());
        assert_eq!(task.signature_id(), unreg_task.signature_id());
    }

    #[quickcheck]
    #[should_panic]
    fn getting_a_task_with_an_unregistered_handle_is_a_panic(task_id: TaskId) {
        let reg = TaskRegistry::new();
        reg.get(task_id);
    }

    #[quickcheck]
    fn updating_a_handle_to_point_to_a_new_task(
        unreg_task_a: UnregisteredTask,
        unreg_task_b: UnregisteredTask,
    ) {
        let reg = TaskRegistry::new();
        let task_a = reg.register(unreg_task_a);
        let task_b = reg.register(unreg_task_b);
        assert_eq!(reg.get(*task_a.id()), task_a);
        let task_b = reg.update(*task_a.id(), task_b.clone());
        assert_eq!(reg.get(*task_a.id()), task_b);
        assert_eq!(reg.get(*task_b.id()), task_b);
    }

    #[quickcheck]
    fn registering_many_tasks_skips_duplicates(unreg_task: UnregisteredTask) {
        let reg = TaskRegistry::new();
        // NOTE(@ostera): we are registering the same task twice
        let unreg_tasks = &[unreg_task.clone(), unreg_task];
        let tasks = reg.register_many(unreg_tasks);
        assert!(tasks.len() <= unreg_tasks.len());
        assert_eq!(tasks[0].id(), tasks[1].id());
    }
}
