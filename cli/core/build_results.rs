use super::*;
use daggy::{Dag, NodeIndex};
use dashmap::DashMap;
use dashmap::DashSet;
use fxhash::*;
use std::sync::Arc;
use thiserror::*;
use tracing::*;

#[derive(Error, Debug)]
pub enum BuildResultError {
    #[error("Dependency cycle found starting at {}", .label.to_string())]
    DepGraphError {
        label: Label,
        inner_error: daggy::WouldCycle<()>,
    },

    #[error(transparent)]
    Unknown(anyhow::Error),
}

#[derive(Debug, Clone)]
pub struct BuildResult {
    pub target_manifest: Arc<TargetManifest>,

    pub executable_target: Arc<ExecutableTarget>,
}

/// A collection of expectations and results from a build.
///
/// This struct is used to keep track of what targets are _expected_ to be built,
/// and which targets are _actually_ built (`build_results`).
///
#[derive(Debug, Default, Clone)]
pub struct BuildResults {
    // The shared state of what targets have been built across workers.
    build_results: Arc<DashMap<LabelId, BuildResult>>,

    expected_targets: Arc<DashSet<LabelId>>,

    missing_targets: Arc<DashSet<LabelId>>,

    build_graph: Arc<DashMap<LabelId, Vec<LabelId>>>,

    label_registry: Arc<LabelRegistry>,

    ready_marker: Arc<DashSet<bool>>,
}

impl BuildResults {
    #[tracing::instrument(name = "BuildResults::new")]
    pub fn new(label_registry: Arc<LabelRegistry>) -> Self {
        Self {
            build_results: Arc::new(DashMap::new()),
            expected_targets: Arc::new(DashSet::new()),
            missing_targets: Arc::new(DashSet::new()),
            build_graph: Arc::new(DashMap::new()),
            label_registry,
            ready_marker: Arc::new(DashSet::new()),
        }
    }

    pub fn mark_as_ready(&self) {
        self.ready_marker.insert(true);
    }

    pub fn result_count(&self) -> usize {
        self.build_results.len()
    }

    pub fn clear_results(&self) {
        self.build_results.clear()
    }

    pub fn get_results(&self) -> Vec<BuildResult> {
        self.build_results
            .iter()
            .map(|r| r.value().clone())
            .collect()
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

    pub fn remove_expected_target(&self, label: LabelId) {
        self.expected_targets.remove(&label);
        self.missing_targets.remove(&label);
    }

    pub fn add_expected_target(&self, label: LabelId) {
        self.expected_targets.insert(label);
        self.missing_targets.insert(label);
    }

    pub fn add_fetch_result(&self, label: LabelId) {
        self.missing_targets.remove(&label);
    }

    pub fn add_computed_target(
        &self,
        label: LabelId,
        manifest: TargetManifest,
        target: ExecutableTarget,
    ) {
        self.missing_targets.remove(&label);
        self.build_results.insert(
            label,
            BuildResult {
                target_manifest: Arc::new(manifest),
                executable_target: Arc::new(target),
            },
        );
    }

    pub fn add_dependencies(
        &self,
        label: LabelId,
        deps: &[LabelId],
    ) -> Result<(), BuildResultError> {
        self.build_graph.insert(label, deps.to_vec());

        let mut dag: Dag<LabelId, (), u32> = Dag::new();

        let mut nodes: FxHashMap<LabelId, NodeIndex> = FxHashMap::default();
        for entry in self.build_graph.iter() {
            let label = *entry.key();
            let node_idx = dag.add_node(label);
            nodes.insert(label, node_idx);
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
            .map_err(|e| BuildResultError::DepGraphError {
                label: (*self.label_registry.get_label(label)).to_owned(),
                inner_error: e,
            })?;

        Ok(())
    }

    pub fn get_build_result(&self, label: LabelId) -> Option<BuildResult> {
        self.build_results.get(&label).map(|r| (*r).clone())
    }

    pub fn get_manifest(&self, label: LabelId) -> Option<Arc<TargetManifest>> {
        self.build_results
            .get(&label)
            .map(|r| (*r).target_manifest.clone())
    }

    pub fn get_target_runtime_deps(&self, label: LabelId) -> Vec<LabelId> {
        self.build_results
            .get(&label)
            .map(|r| (*r).executable_target.runtime_deps.clone())
            .unwrap_or_default()
    }

    pub fn get_target_deps(&self, label: LabelId) -> Vec<LabelId> {
        self.build_results
            .get(&label)
            .map(|r| (*r).executable_target.deps.clone())
            .unwrap_or_default()
    }

    pub fn is_label_built(&self, label: LabelId) -> bool {
        self.build_results.contains_key(&label)
    }
}

#[cfg(test)]
mod tests {

    use std::{collections::BTreeMap, path::PathBuf};

    use super::*;

    async fn make_target(label: Label) -> ExecutableTarget {
        let rule = Rule::new(
            "test_rule".to_string(),
            "TestRule".to_string(),
            vec![],
            ConfigSpec::default(),
            RuleConfig::default(),
            Runnable::NotRunnable,
            Pinned::Pinned,
            Portability::Portable,
        );
        let cfg = RuleConfig::default();
        let target = Target::new(label, &rule.name, cfg);
        ExecutableTarget::new(
            &ExecutionEnvironment::new(),
            &rule,
            &target,
            &[],
            &[],
            &[],
            ExecutionResult::default(),
        )
        .await
        .unwrap()
    }

    async fn make_manifest(target: &ExecutableTarget) -> TargetManifest {
        TargetManifest::from_validation_result(
            chrono::Utc::now(),
            &ValidationStatus::Cached,
            &PathBuf::from("."),
            BTreeMap::default(),
            target,
        )
    }

    #[test]
    fn an_empty_build_results_never_has_all_its_expected_targets() {
        let br = BuildResults::new();
        assert!(!br.has_all_expected_targets());
    }

    #[tokio::test]
    async fn build_results_needs_expected_and_completed_targets_to_match() {
        let br = BuildResults::new();

        let label = Label::new("//test/0");
        br.add_expected_target(label.clone());
        assert!(!br.has_all_expected_targets());

        let bad_label = Label::new("//test/another-label");
        let target = make_target(bad_label.clone()).await;
        let manifest = make_manifest(&target).await;
        br.add_computed_target(bad_label.clone(), manifest, target);
        assert!(!br.has_all_expected_targets());

        let target = make_target(label.clone()).await;
        let manifest = make_manifest(&target).await;
        br.add_computed_target(label.clone(), manifest, target);
        assert!(br.has_all_expected_targets());

        let new_label = Label::new("//test/1");
        br.add_expected_target(new_label);
        assert!(!br.has_all_expected_targets());
    }

    #[tokio::test]
    async fn after_adding_a_computed_target_we_can_ask_if_it_is_built() {
        let br = BuildResults::new();

        let label = Label::new("//test/0");

        assert!(!br.is_target_built(&label));

        let target = make_target(label.clone()).await;
        let manifest = make_manifest(&target).await;
        br.add_computed_target(label.clone(), manifest, target);
        assert!(br.is_target_built(&label));
    }

    #[tokio::test]
    async fn after_adding_a_computed_target_we_can_fetch_it() {
        let br = BuildResults::new();

        let label = Label::new("//test/0");

        assert!(br.get_computed_target(&label).is_none());

        let target = make_target(label.clone()).await;
        let manifest = make_manifest(&target).await;
        br.add_computed_target(label.clone(), manifest, target);
        assert!(br.get_computed_target(&label).is_some());
    }

    #[test]
    fn detects_cycle_when_adding_deps() {
        let br = BuildResults::new();

        let label_a = Label::new("//a");
        let label_b = Label::new("//b");

        let deps_a = vec![label_b.clone()];
        let deps_b = vec![label_a.clone()];

        assert!(br.add_dependencies(label_a, &deps_a).is_ok());
        assert_matches!(
            br.add_dependencies(label_b, &deps_b),
            Err(BuildResultError::DepGraphError {
                label: _,
                inner_error: daggy::WouldCycle(_)
            })
        );
    }
}
