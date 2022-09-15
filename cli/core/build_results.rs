use super::*;
use daggy::{Dag, NodeIndex};
use dashmap::DashMap;
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

/// A collection of expectations and results from a build.
///
/// This struct is used to keep track of what targets are _expected_ to be built,
/// and which targets are _actually_ built (`computed_targets`).
///
#[derive(Debug)]
pub struct BuildResults {
    // The shared state of what targets have been built across workers.
    pub computed_targets: Arc<DashMap<Label, ExecutableTarget>>,

    pub expected_targets: Arc<DashMap<Label, ()>>,

    pub missing_targets: Arc<DashMap<Label, ()>>,

    pub build_graph: Arc<DashMap<Label, Vec<Label>>>,
}

impl BuildResults {
    #[tracing::instrument(name = "BuildResults::new")]
    pub fn new() -> BuildResults {
        BuildResults {
            computed_targets: Arc::new(DashMap::new()),
            expected_targets: Arc::new(DashMap::new()),
            missing_targets: Arc::new(DashMap::new()),
            build_graph: Arc::new(DashMap::new()),
        }
    }

    // NOTE(@ostera): there's plenty better ways of computing this, one would be to keep 2 maps for
    // expected targets, and remove entries from it as entries are added to the computed targets.
    // That way that map represents the current state of the build results, and we can just check
    // if it is empty to see if we're done.
    //
    pub fn has_all_expected_targets(&self) -> bool {
        if self.expected_targets.is_empty() {
            return false;
        }

        self.missing_targets.is_empty()
    }

    pub fn add_expected_target(&self, label: Label) {
        self.expected_targets.insert(label.clone(), ());
        self.missing_targets.insert(label, ());
    }

    pub fn add_computed_target(&self, label: &Label, target: ExecutableTarget) {
        self.missing_targets.remove(&label);
        self.computed_targets.insert(label.clone(), target);
    }

    pub fn add_dependencies(&self, label: Label, deps: &[Label]) -> Result<(), BuildResultError> {
        self.build_graph.insert(label.clone(), deps.to_vec());

        let mut dag: Dag<Label, (), u32> = Dag::new();

        let mut nodes: FxHashMap<Label, NodeIndex> = FxHashMap::default();
        for entry in self.build_graph.iter() {
            let label = entry.key().clone();
            let node_idx = dag.add_node(label.clone());
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
                label,
                inner_error: e,
            })?;

        Ok(())
    }

    pub fn get_computed_target(&self, label: &Label) -> Option<ExecutableTarget> {
        self.computed_targets.get(label).map(|n| n.clone())
    }

    pub fn is_target_built(&self, label: &Label) -> bool {
        self.computed_targets.contains_key(label)
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    async fn dummy_target(label: Label) -> ExecutableTarget {
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
            ExecutionResult::default(),
        )
        .await
        .unwrap()
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
        let target = dummy_target(bad_label.clone()).await;
        br.add_computed_target(bad_label.clone(), target);
        assert!(!br.has_all_expected_targets());

        let target = dummy_target(label.clone()).await;
        br.add_computed_target(label.clone(), target);
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

        let target = dummy_target(label.clone()).await;
        br.add_computed_target(label.clone(), target);
        assert!(br.is_target_built(&label));
    }

    #[tokio::test]
    async fn after_adding_a_computed_target_we_can_fetch_it() {
        let br = BuildResults::new();

        let label = Label::new("//test/0");

        assert!(br.get_computed_target(&label).is_none());

        let target = dummy_target(label.clone()).await;
        br.add_computed_target(label.clone(), target);
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
