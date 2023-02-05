use crate::resolver::*;
use daggy::{Dag, NodeIndex};
use dashmap::DashMap;
use dashmap::DashSet;
use fxhash::*;
use std::sync::Arc;
use thiserror::*;
use tracing::*;

#[derive(Error, Debug)]
pub enum TaskResultError {
    #[error("Dependency cycle found starting at {}", .target.to_string())]
    DepGraphError {
        target: Target,
        inner_error: daggy::WouldCycle<()>,
    },

    #[error(transparent)]
    Unknown(anyhow::Error),
}

#[derive(Debug, Clone)]
pub struct TaskResult {
    pub target_manifest: Arc<()>,
    pub executable_target: Arc<()>,
}

/// A collection of expectations and results from a build.
///
/// This struct is used to keep track of what targets are _expected_ to be built,
/// and which targets are _actually_ built (`results`).
///
#[derive(Debug, Default, Clone)]
pub struct TaskResults {
    // The shared state of what targets have been built across workers.
    results: Arc<DashMap<TargetId, TaskResult>>,

    expected_targets: Arc<DashSet<TargetId>>,

    missing_targets: Arc<DashSet<TargetId>>,

    build_graph: Arc<DashMap<TargetId, Vec<TargetId>>>,

    target_registry: Arc<TargetRegistry>,

    ready_marker: Arc<DashSet<bool>>,
}

impl TaskResults {
    #[tracing::instrument(name = "TaskResults::new")]
    pub fn new(target_registry: Arc<TargetRegistry>) -> Self {
        Self {
            results: Arc::new(DashMap::new()),
            expected_targets: Arc::new(DashSet::new()),
            missing_targets: Arc::new(DashSet::new()),
            build_graph: Arc::new(DashMap::new()),
            target_registry,
            ready_marker: Arc::new(DashSet::new()),
        }
    }

    pub fn mark_as_ready(&self) {
        self.ready_marker.insert(true);
    }

    pub fn result_count(&self) -> usize {
        self.results.len()
    }

    pub fn clear_results(&self) {
        self.results.clear()
    }

    pub fn get_results(&self) -> Vec<TaskResult> {
        self.results.iter().map(|r| r.value().clone()).collect()
    }

    // NOTE(@ostera): there's plenty better ways of computing this, one would be to keep 2 maps for
    // expected targets, and remove entries from it as entries are added to the computed targets.
    // That way that map represents the current state of the build results, and we can just check
    // if it is empty to see if we're done.
    //
    pub fn has_all_expected_targets(&self) -> bool {
        if self.ready_marker.is_empty() || self.expected_targets.is_empty() {
            return false;
        }

        self.missing_targets.is_empty()
    }

    pub fn remove_expected_target(&self, target: TargetId) {
        self.expected_targets.remove(&target);
        self.missing_targets.remove(&target);
    }

    pub fn add_expected_target(&self, target: TargetId) {
        self.expected_targets.insert(target);
        self.missing_targets.insert(target);
    }

    pub fn add_target_manifest(
        &self,
        target: TargetId,
        executable_target: (),
        target_manifest: (),
    ) {
        self.missing_targets.remove(&target);
        self.results.insert(
            target,
            TaskResult {
                target_manifest: Arc::new(target_manifest),
                executable_target: Arc::new(executable_target),
            },
        );
    }

    pub fn add_dependencies(
        &self,
        target: TargetId,
        deps: &[TargetId],
    ) -> Result<(), TaskResultError> {
        self.build_graph.insert(target, deps.to_vec());

        let mut dag: Dag<TargetId, (), u32> = Dag::new();

        let mut nodes: FxHashMap<TargetId, NodeIndex> = FxHashMap::default();
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

        dag.extend_with_edges(edges)
            .map_err(|e| TaskResultError::DepGraphError {
                target: (*self.target_registry.get_target(target)).to_owned(),
                inner_error: e,
            })?;

        Ok(())
    }

    pub fn get_build_result(&self, target: TargetId) -> Option<TaskResult> {
        self.results.get(&target).map(|r| (*r).clone())
    }

    /*
    pub fn get_manifest(&self, target: TargetId) -> Option<Arc<TargetManifest>> {
        self.results.get(&target).map(|r| r.target_manifest.clone())
    }
    */

    /*
    pub fn get_target_runtime_deps(&self, target: TargetId) -> Vec<TargetId> {
        self.results
            .get(&target)
            .map(|r| r.executable_target.runtime_deps.clone())
            .unwrap_or_default()
    }
    */

    /*
    pub fn get_target_deps(&self, target: TargetId) -> Vec<TargetId> {
        self.results
            .get(&target)
            .map(|r| r.executable_target.deps.clone())
            .unwrap_or_default()
    }
    */

    pub fn is_target_built(&self, target: TargetId) -> bool {
        self.results.contains_key(&target)
    }
}

#[cfg(test)]
mod tests {}
