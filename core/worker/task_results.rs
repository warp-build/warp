use crate::model::{ExecutableSpec, Signature, Target, TargetId, Task, TaskId};
use crate::resolver::{SignatureRegistry, TargetRegistry};
use crate::store::ArtifactManifest;
use crate::sync::Arc;
use daggy::{Dag, NodeIndex};
use dashmap::{DashMap, DashSet};
use fxhash::*;
use thiserror::*;
use tracing::{instrument, *};

use super::TaskRegistry;

#[derive(Debug, Clone)]
pub struct TaskResult {
    pub task: Task,
    pub artifact_manifest: Arc<ArtifactManifest>,
    pub executable_spec: Arc<ExecutableSpec>,
}

/// A collection of expectations and results from a build.
///
/// This struct is used to keep track of what targets are _expected_ to be built,
/// and which targets are _actually_ built (`results`).
///
#[derive(Debug, Default, Clone)]
pub struct TaskResults {
    // The shared state of what targets have been built across workers.
    results: Arc<DashMap<TaskId, TaskResult>>,

    expected_tasks: Arc<DashSet<TaskId>>,

    missing_tasks: Arc<DashSet<TaskId>>,

    build_graph: Arc<DashMap<TaskId, Vec<TaskId>>>,

    task_registry: Arc<TaskRegistry>,

    target_registry: Arc<TargetRegistry>,

    signature_registry: Arc<SignatureRegistry>,

    ready_marker: Arc<DashSet<bool>>,
}

impl TaskResults {
    #[instrument(name = "TaskResults::new", skip(target_registry, signature_registry))]
    pub fn new(
        task_registry: Arc<TaskRegistry>,
        target_registry: Arc<TargetRegistry>,
        signature_registry: Arc<SignatureRegistry>,
    ) -> Self {
        Self {
            results: Arc::new(DashMap::new()),
            expected_tasks: Arc::new(DashSet::new()),
            missing_tasks: Arc::new(DashSet::new()),
            build_graph: Arc::new(DashMap::new()),
            target_registry,
            task_registry,
            signature_registry,
            ready_marker: Arc::new(DashSet::new()),
        }
    }

    pub fn mark_as_ready(&self) {
        self.ready_marker.insert(true);
    }

    pub fn len(&self) -> usize {
        self.results.len()
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn clear(&self) {
        self.results.clear()
    }

    pub fn get_results(&self) -> Vec<TaskResult> {
        self.results.iter().map(|r| r.value().clone()).collect()
    }

    pub fn has_all_expected_targets(&self) -> bool {
        if self.ready_marker.is_empty() {
            return false;
        }

        dbg!(&self
            .results
            .iter()
            .map(|e| *e.key())
            .collect::<Vec<TaskId>>());
        dbg!(&self.missing_tasks);

        self.missing_tasks.is_empty()
    }

    pub fn remove_expected_task(&self, task: Task) {
        self.expected_tasks.remove(task.id());
        self.missing_tasks.remove(task.id());
    }

    pub fn add_expected_task(&self, task: Task) {
        self.expected_tasks.insert(*task.id());
        self.missing_tasks.insert(*task.id());
    }

    pub fn add_task_result(
        &self,
        task: Task,
        executable_spec: ExecutableSpec,
        artifact_manifest: ArtifactManifest,
    ) {
        self.missing_tasks.remove(task.id());
        self.results.insert(
            *task.id(),
            TaskResult {
                task,
                artifact_manifest: Arc::new(artifact_manifest),
                executable_spec: Arc::new(executable_spec),
            },
        );
    }

    pub fn add_dependencies(&self, task: TaskId, deps: &[TaskId]) -> Result<(), TaskResultError> {
        let deps = if let Some(old_deps) = self.build_graph.get(&task) {
            (*old_deps)
                .iter()
                .chain(deps.iter())
                .copied()
                .collect::<FxHashSet<TaskId>>()
                .into_iter()
                .collect::<Vec<TaskId>>()
        } else {
            deps.to_vec()
        };

        self.build_graph.insert(task, deps);

        let mut dag: Dag<TaskId, (), u32> = Dag::new();

        let mut nodes: FxHashMap<TaskId, NodeIndex> = FxHashMap::default();
        for entry in self.build_graph.iter() {
            let target = *entry.key();
            let node_idx = dag.add_node(target);
            nodes.insert(target, node_idx);
        }

        let mut edges = vec![];
        for entry in self.build_graph.iter() {
            if let Some(node_idx) = nodes.get(entry.key()) {
                for dep in entry.value() {
                    if let Some(dep_idx) = nodes.get(dep) {
                        edges.push((*dep_idx, *node_idx));
                    }
                }
            }
        }

        let task = self.task_registry.get(task);
        dag.extend_with_edges(edges)
            .map_err(|e| TaskResultError::DepGraphError {
                target: self.target_registry.get_target(task.target_id()),
                signature: task
                    .signature_id()
                    .map(|sig_id| self.signature_registry.get(sig_id)),
                inner_error: e,
            })?;

        Ok(())
    }

    #[instrument(name = "TaskResults::get_task_deps", skip(self))]
    pub fn get_task_deps(
        &self,
        task: Task,
    ) -> Vec<(Task, Arc<ExecutableSpec>, Arc<ArtifactManifest>)> {
        self.build_graph
            .get(task.id())
            .as_ref()
            .map(|deps| {
                (*deps)
                    .iter()
                    .flat_map(|dep| self.results.get(&dep).map(|r| (*r).clone()))
                    .map(|result| {
                        (
                            result.task,
                            result.executable_spec,
                            result.artifact_manifest,
                        )
                    })
                    .collect()
            })
            .unwrap_or_default()
    }

    pub fn get_task_result(&self, task: &Task) -> Option<TaskResult> {
        self.results.get(task.id()).map(|r| (*r).clone())
    }

    pub fn is_task_completed(&self, task: Task) -> bool {
        self.results.contains_key(task.id())
    }

    pub fn find_task_by_target_id(&self, target_id: TargetId) -> Option<Task> {
        for result in self.results.iter() {
            if result.executable_spec.target().target_id() == target_id {
                let task = self.task_registry.get(*result.key());
                return Some(task);
            }
        }
        None
    }
}

#[derive(Error, Debug)]
pub enum TaskResultError {
    #[error("Dependency cycle found starting at {}", .target.to_string())]
    DepGraphError {
        target: Arc<Target>,
        inner_error: daggy::WouldCycle<()>,
        signature: Option<Arc<Signature>>,
    },

    #[error(transparent)]
    Unknown(anyhow::Error),
}

#[cfg(test)]
mod tests {}
