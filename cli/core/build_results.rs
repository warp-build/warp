use super::*;
use dashmap::DashMap;
use std::sync::Arc;
use tracing::*;

/// A collection of expectations and results from a build.
///
/// This struct is used to keep track of what targets are _expected_ to be built,
/// and which targets are _actually_ built (`computed_targets`).
///
#[derive(Debug)]
pub struct BuildResults {
    // The shared state of what targets have been built across workers.
    pub computed_targets: Arc<DashMap<Label, ComputedTarget>>,

    pub expected_targets: Arc<DashMap<Label, ()>>,
}

impl BuildResults {
    #[tracing::instrument(name = "BuildResults::new")]
    pub fn new() -> BuildResults {
        BuildResults {
            computed_targets: Arc::new(DashMap::new()),
            expected_targets: Arc::new(DashMap::new()),
        }
    }

    // NOTE(@ostera): there's plenty better ways of computing this, one would be to keep 2 maps for
    // expected targets, and remove entries from it as entries are added to the computed targets.
    // That way that map represents the current state of the build results, and we can just check
    // if it is empty to see if we're done.
    //
    pub fn has_all_expected_targets(&self) -> bool {
        if self.expected_targets.is_empty() {
            return false
        }

        for expected in self.expected_targets.iter() {
            if !self.computed_targets.contains_key(expected.key()) {
                return false;
            }
        }

        true
    }

    pub fn add_expected_target(&self, label: Label) {
        self.expected_targets.insert(label, ());
    }

    pub fn is_target_built(&self, label: &Label) -> bool {
        self.computed_targets.contains_key(&label)
    }

    pub fn add_computed_target(&self, label: Label, target: ComputedTarget) {
        self.computed_targets.insert(label, target);
    }

    pub fn get_computed_target(&self, label: &Label) -> Option<ComputedTarget> {
        self.computed_targets.get(&label).map(|n| n.clone()).clone()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn dummy_target(label: Label) -> ComputedTarget {
        let rule = Rule::new(
            "test_rule".to_string(),
            "TestRule".to_string(),
            vec![],
            ConfigSpec::default(),
            RuleConfig::default(),
            false,
        );
        let cfg = RuleConfig::default();
        let target = Target::local(label, &rule, cfg);
        ComputedTarget::from_target(target)
    }

    #[test]
    fn an_empty_build_results_never_has_all_its_expected_targets() {
        let br = BuildResults::new();
        assert!(!br.has_all_expected_targets());
    }

    #[test]
    fn build_results_needs_expected_and_completed_targets_to_match() {
        let br = BuildResults::new();

        let label = Label::new("//test/0");
        br.add_expected_target(label.clone());
        assert!(!br.has_all_expected_targets());

        let bad_label = Label::new("//test/another-label");
        let target = dummy_target(bad_label.clone());
        br.add_computed_target(bad_label.clone(), target);
        assert!(!br.has_all_expected_targets());

        let target = dummy_target(label.clone());
        br.add_computed_target(label.clone(), target);
        assert!(br.has_all_expected_targets());

        let new_label = Label::new("//test/1");
        br.add_expected_target(new_label.clone());
        assert!(!br.has_all_expected_targets());
    }

    #[test]
    fn after_adding_a_computed_target_we_can_ask_if_it_is_built() {
        let br = BuildResults::new();

        let label = Label::new("//test/0");

        assert!(!br.is_target_built(&label));

        let target = dummy_target(label.clone());
        br.add_computed_target(label.clone(), target);
        assert!(br.is_target_built(&label));
    }

    #[test]
    fn after_adding_a_computed_target_we_can_fetch_it() {
        let br = BuildResults::new();

        let label = Label::new("//test/0");

        assert!(br.get_computed_target(&label).is_none());

        let target = dummy_target(label.clone());
        br.add_computed_target(label.clone(), target);
        assert!(br.get_computed_target(&label).is_some());
    }

}
