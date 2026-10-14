use std::collections::{BTreeMap, HashMap, HashSet, VecDeque};
use std::fmt;
use std::num::NonZeroUsize;
use std::sync::{Arc, Condvar, Mutex};
use std::thread;

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TaskId(String);

impl TaskId {
    pub fn new(value: impl Into<String>) -> Result<Self, GraphError> {
        let value = value.into();
        if value.trim().is_empty() {
            return Err(GraphError::InvalidTaskId);
        }
        Ok(Self(value))
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl fmt::Display for TaskId {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(&self.0)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ActionPlan {
    pub description: String,
    pub dependencies: Vec<TaskId>,
}

impl ActionPlan {
    pub fn new(description: impl Into<String>, dependencies: Vec<TaskId>) -> Self {
        Self {
            description: description.into(),
            dependencies,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Plan {
    NeedsDependencies(Vec<TaskId>),
    Ready(ActionPlan),
}

pub trait TaskProvider: Sync {
    fn plan(&self, task: &TaskId, completed_dependencies: &[TaskId]) -> Result<Plan, String>;
    fn execute(&self, task: &TaskId, action: &ActionPlan) -> Result<(), String>;
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum GraphError {
    InvalidTaskId,
    NoRootTasks,
    PlannerFailed { task: TaskId, reason: String },
    ExecutorFailed { task: TaskId, reason: String },
    MissingDependencyListEmpty { task: TaskId },
    PlannerMadeNoProgress { task: TaskId },
    DependencyCycle { task: TaskId, dependency: TaskId },
    Deadlock { waiting: Vec<TaskId> },
    WorkerPanicked,
}

impl fmt::Display for GraphError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidTaskId => formatter.write_str("task id must not be empty"),
            Self::NoRootTasks => {
                formatter.write_str("graph execution requires at least one root task")
            }
            Self::PlannerFailed { task, reason } => {
                write!(formatter, "planning task {task} failed: {reason}")
            }
            Self::ExecutorFailed { task, reason } => {
                write!(formatter, "executing task {task} failed: {reason}")
            }
            Self::MissingDependencyListEmpty { task } => {
                write!(
                    formatter,
                    "planner for task {task} requested an empty dependency set"
                )
            }
            Self::PlannerMadeNoProgress { task } => {
                write!(
                    formatter,
                    "planner for task {task} requested only completed dependencies"
                )
            }
            Self::DependencyCycle { task, dependency } => {
                write!(
                    formatter,
                    "adding edge {task} -> {dependency} creates a dependency cycle"
                )
            }
            Self::Deadlock { waiting } => write!(
                formatter,
                "graph has no runnable tasks but dependencies remain: {}",
                waiting
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Self::WorkerPanicked => formatter.write_str("a graph worker thread panicked"),
        }
    }
}

impl std::error::Error for GraphError {}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ExecutionReport {
    pub completed: Vec<TaskId>,
    pub planning_attempts: BTreeMap<TaskId, usize>,
    pub actions: BTreeMap<TaskId, String>,
}

pub struct GraphExecutor {
    worker_count: NonZeroUsize,
}

impl GraphExecutor {
    pub fn new(worker_count: NonZeroUsize) -> Self {
        Self { worker_count }
    }

    pub fn execute(
        &self,
        provider: &impl TaskProvider,
        roots: impl IntoIterator<Item = TaskId>,
    ) -> Result<ExecutionReport, GraphError> {
        let shared = Arc::new(SharedState::default());
        {
            let mut state = shared.state.lock().expect("graph state lock poisoned");
            for root in roots {
                enqueue(&mut state, root);
            }
            if state.nodes.is_empty() {
                return Err(GraphError::NoRootTasks);
            }
        }

        let workers_panicked = thread::scope(|scope| {
            let workers = (0..self.worker_count.get())
                .map(|_| {
                    let shared = Arc::clone(&shared);
                    scope.spawn(move || worker_loop(provider, shared))
                })
                .collect::<Vec<_>>();
            workers.into_iter().any(|worker| worker.join().is_err())
        });
        if workers_panicked {
            return Err(GraphError::WorkerPanicked);
        }

        let mut state = shared.state.lock().expect("graph state lock poisoned");
        if let Some(error) = state.failure.take() {
            return Err(error);
        }

        let mut completed: Vec<_> = state
            .nodes
            .iter()
            .filter_map(|(task, node)| (node.status == Status::Complete).then_some(task.clone()))
            .collect();
        completed.sort();
        Ok(ExecutionReport {
            completed,
            planning_attempts: state
                .nodes
                .iter()
                .map(|(task, node)| (task.clone(), node.planning_attempts))
                .collect(),
            actions: state
                .nodes
                .iter()
                .filter_map(|(task, node)| {
                    node.action
                        .as_ref()
                        .map(|action| (task.clone(), action.description.clone()))
                })
                .collect(),
        })
    }
}

#[derive(Default)]
struct SharedState {
    state: Mutex<State>,
    changed: Condvar,
}

#[derive(Default)]
struct State {
    nodes: HashMap<TaskId, Node>,
    ready: VecDeque<TaskId>,
    active_workers: usize,
    failure: Option<GraphError>,
}

#[derive(Default)]
struct Node {
    status: Status,
    dependencies: HashSet<TaskId>,
    dependents: HashSet<TaskId>,
    planning_attempts: usize,
    action: Option<ActionPlan>,
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
enum Status {
    #[default]
    Queued,
    Planning,
    Waiting,
    Running,
    Complete,
}

fn enqueue(state: &mut State, task: TaskId) {
    let node = state.nodes.entry(task.clone()).or_default();
    if node.status == Status::Queued {
        if !state.ready.contains(&task) {
            state.ready.push_back(task);
        }
    } else if node.status == Status::Waiting {
        node.status = Status::Queued;
        state.ready.push_back(task);
    }
}

fn worker_loop(provider: &impl TaskProvider, shared: Arc<SharedState>) {
    loop {
        let (task, completed_dependencies) = match next_task(&shared) {
            Some(task) => task,
            None => return,
        };

        let plan = match provider.plan(&task, &completed_dependencies) {
            Ok(plan) => plan,
            Err(reason) => {
                fail(&shared, GraphError::PlannerFailed { task, reason });
                continue;
            }
        };
        match accept_plan(&shared, &task, plan) {
            PlanDecision::Deferred => continue,
            PlanDecision::Failed(error) => {
                fail(&shared, error);
                continue;
            }
            PlanDecision::Execute(action) => {
                if let Err(reason) = provider.execute(&task, &action) {
                    fail(&shared, GraphError::ExecutorFailed { task, reason });
                } else {
                    complete(&shared, task, action);
                }
            }
        }
    }
}

fn next_task(shared: &SharedState) -> Option<(TaskId, Vec<TaskId>)> {
    let mut state = shared.state.lock().expect("graph state lock poisoned");
    loop {
        if state.failure.is_some() {
            return None;
        }
        if let Some(task) = state.ready.pop_front() {
            let node = state.nodes.get(&task).expect("ready task has a graph node");
            if node.status != Status::Queued {
                continue;
            }
            let completed_dependencies = node
                .dependencies
                .iter()
                .filter(|dependency| state.nodes[*dependency].status == Status::Complete)
                .cloned()
                .collect();
            let node = state.nodes.get_mut(&task).unwrap();
            node.status = Status::Planning;
            node.planning_attempts += 1;
            state.active_workers += 1;
            return Some((task, completed_dependencies));
        }
        if state
            .nodes
            .values()
            .all(|node| node.status == Status::Complete)
        {
            return None;
        }
        if state.active_workers == 0 {
            let mut waiting: Vec<_> = state
                .nodes
                .iter()
                .filter_map(|(task, node)| {
                    (node.status != Status::Complete).then_some(task.clone())
                })
                .collect();
            waiting.sort();
            state.failure = Some(GraphError::Deadlock { waiting });
            shared.changed.notify_all();
            return None;
        }
        state = shared
            .changed
            .wait(state)
            .expect("graph state lock poisoned while waiting");
    }
}

fn accept_plan(shared: &SharedState, task: &TaskId, plan: Plan) -> PlanDecision {
    let mut state = shared.state.lock().expect("graph state lock poisoned");
    if state.failure.is_some() {
        finish_worker(shared, &mut state);
        return PlanDecision::Deferred;
    }

    let (dependencies, action) = match plan {
        Plan::NeedsDependencies(dependencies) if dependencies.is_empty() => {
            return PlanDecision::Failed(GraphError::MissingDependencyListEmpty {
                task: task.clone(),
            });
        }
        Plan::NeedsDependencies(dependencies) => (dependencies, None),
        Plan::Ready(action) => (action.dependencies.clone(), Some(action)),
    };

    for dependency in &dependencies {
        if creates_cycle(&state, task, dependency) {
            return PlanDecision::Failed(GraphError::DependencyCycle {
                task: task.clone(),
                dependency: dependency.clone(),
            });
        }
    }

    for dependency in dependencies {
        state.nodes.entry(dependency.clone()).or_default();
        state
            .nodes
            .get_mut(task)
            .expect("planned task has a graph node")
            .dependencies
            .insert(dependency.clone());
        state
            .nodes
            .get_mut(&dependency)
            .expect("dependency task was inserted")
            .dependents
            .insert(task.clone());
        enqueue(&mut state, dependency);
    }

    let all_dependencies_complete = state
        .nodes
        .get(task)
        .expect("planned task has a graph node")
        .dependencies
        .iter()
        .all(|dependency| state.nodes[dependency].status == Status::Complete);

    match action {
        None if all_dependencies_complete => {
            PlanDecision::Failed(GraphError::PlannerMadeNoProgress { task: task.clone() })
        }
        None => {
            state.nodes.get_mut(task).unwrap().status = Status::Waiting;
            finish_worker(shared, &mut state);
            shared.changed.notify_all();
            PlanDecision::Deferred
        }
        Some(action) if !all_dependencies_complete => {
            let node = state.nodes.get_mut(task).unwrap();
            node.action = Some(action);
            node.status = Status::Waiting;
            finish_worker(shared, &mut state);
            shared.changed.notify_all();
            PlanDecision::Deferred
        }
        Some(action) => {
            let node = state.nodes.get_mut(task).unwrap();
            node.action = Some(action.clone());
            node.status = Status::Running;
            PlanDecision::Execute(action)
        }
    }
}

fn creates_cycle(state: &State, task: &TaskId, dependency: &TaskId) -> bool {
    if task == dependency {
        return true;
    }
    let mut pending = vec![dependency.clone()];
    let mut visited = HashSet::new();
    while let Some(current) = pending.pop() {
        if &current == task {
            return true;
        }
        if !visited.insert(current.clone()) {
            continue;
        }
        if let Some(node) = state.nodes.get(&current) {
            pending.extend(node.dependencies.iter().cloned());
        }
    }
    false
}

fn complete(shared: &SharedState, task: TaskId, action: ActionPlan) {
    let mut state = shared.state.lock().expect("graph state lock poisoned");
    let dependents = {
        let node = state.nodes.get_mut(&task).expect("completed task exists");
        node.status = Status::Complete;
        node.action = Some(action);
        node.dependents.iter().cloned().collect::<Vec<_>>()
    };
    finish_worker(shared, &mut state);
    for dependent in dependents {
        let ready = {
            let node = &state.nodes[&dependent];
            node.status == Status::Waiting
                && node
                    .dependencies
                    .iter()
                    .all(|dependency| state.nodes[dependency].status == Status::Complete)
        };
        if ready {
            enqueue(&mut state, dependent);
        }
    }
    shared.changed.notify_all();
}

fn fail(shared: &SharedState, error: GraphError) {
    let mut state = shared.state.lock().expect("graph state lock poisoned");
    finish_worker(shared, &mut state);
    if state.failure.is_none() {
        state.failure = Some(error);
    }
    shared.changed.notify_all();
}

fn finish_worker(shared: &SharedState, state: &mut State) {
    state.active_workers = state.active_workers.saturating_sub(1);
    shared.changed.notify_all();
}

enum PlanDecision {
    Deferred,
    Execute(ActionPlan),
    Failed(GraphError),
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::time::Duration;

    struct RestartingProvider {
        root_plans: AtomicUsize,
        execution_order: Mutex<Vec<TaskId>>,
    }

    impl TaskProvider for RestartingProvider {
        fn plan(&self, task: &TaskId, completed_dependencies: &[TaskId]) -> Result<Plan, String> {
            if task.as_str() == "root" && self.root_plans.fetch_add(1, Ordering::SeqCst) == 0 {
                return Ok(Plan::NeedsDependencies(vec![
                    TaskId::new("generated").unwrap()
                ]));
            }
            Ok(Plan::Ready(ActionPlan::new(
                task.as_str(),
                completed_dependencies.to_vec(),
            )))
        }

        fn execute(&self, task: &TaskId, _action: &ActionPlan) -> Result<(), String> {
            self.execution_order.lock().unwrap().push(task.clone());
            Ok(())
        }
    }

    #[test]
    fn replans_after_new_dependencies_complete() {
        let provider = RestartingProvider {
            root_plans: AtomicUsize::new(0),
            execution_order: Mutex::new(Vec::new()),
        };
        let report = GraphExecutor::new(NonZeroUsize::new(2).unwrap())
            .execute(&provider, [TaskId::new("root").unwrap()])
            .unwrap();

        assert_eq!(report.planning_attempts[&TaskId::new("root").unwrap()], 2);
        assert_eq!(
            *provider.execution_order.lock().unwrap(),
            vec![
                TaskId::new("generated").unwrap(),
                TaskId::new("root").unwrap()
            ]
        );
    }

    struct ParallelProvider {
        active: AtomicUsize,
        maximum_active: AtomicUsize,
    }

    impl TaskProvider for ParallelProvider {
        fn plan(&self, task: &TaskId, _completed_dependencies: &[TaskId]) -> Result<Plan, String> {
            Ok(Plan::Ready(ActionPlan::new(task.as_str(), vec![])))
        }

        fn execute(&self, _task: &TaskId, _action: &ActionPlan) -> Result<(), String> {
            let active = self.active.fetch_add(1, Ordering::SeqCst) + 1;
            self.maximum_active.fetch_max(active, Ordering::SeqCst);
            thread::sleep(Duration::from_millis(50));
            self.active.fetch_sub(1, Ordering::SeqCst);
            Ok(())
        }
    }

    #[test]
    fn independent_actions_execute_on_multiple_workers() {
        let provider = ParallelProvider {
            active: AtomicUsize::new(0),
            maximum_active: AtomicUsize::new(0),
        };
        let roots = ["one", "two", "three", "four"]
            .into_iter()
            .map(|name| TaskId::new(name).unwrap());
        GraphExecutor::new(NonZeroUsize::new(4).unwrap())
            .execute(&provider, roots)
            .unwrap();
        assert!(provider.maximum_active.load(Ordering::SeqCst) > 1);
    }

    struct CycleProvider;

    impl TaskProvider for CycleProvider {
        fn plan(&self, task: &TaskId, _completed_dependencies: &[TaskId]) -> Result<Plan, String> {
            let dependency = if task.as_str() == "one" { "two" } else { "one" };
            Ok(Plan::NeedsDependencies(vec![
                TaskId::new(dependency).unwrap()
            ]))
        }

        fn execute(&self, _task: &TaskId, _action: &ActionPlan) -> Result<(), String> {
            unreachable!("cyclic tasks must not execute")
        }
    }

    #[test]
    fn rejects_cycles_in_dynamically_discovered_edges() {
        let error = GraphExecutor::new(NonZeroUsize::new(2).unwrap())
            .execute(&CycleProvider, [TaskId::new("one").unwrap()])
            .unwrap_err();
        assert!(matches!(error, GraphError::DependencyCycle { .. }));
    }

    struct NoProgressProvider;

    impl TaskProvider for NoProgressProvider {
        fn plan(&self, _task: &TaskId, _completed_dependencies: &[TaskId]) -> Result<Plan, String> {
            Ok(Plan::NeedsDependencies(vec![]))
        }
        fn execute(&self, _task: &TaskId, _action: &ActionPlan) -> Result<(), String> {
            unreachable!()
        }
    }

    #[test]
    fn rejects_empty_dynamic_dependency_sets() {
        let error = GraphExecutor::new(NonZeroUsize::new(1).unwrap())
            .execute(&NoProgressProvider, [TaskId::new("one").unwrap()])
            .unwrap_err();
        assert!(matches!(
            error,
            GraphError::MissingDependencyListEmpty { .. }
        ));
    }
}
