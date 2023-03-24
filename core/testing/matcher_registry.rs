use crate::model::{TestMatcher, TestMatcherId};
use crate::sync::Arc;
use dashmap::DashMap;
use tracing::*;

/// A small registry to trade test matcher (which are non-Copyable objects) for Copyable
/// Ids.
///
#[derive(Default, Debug, Clone)]
pub struct TestMatcherRegistry {
    specs: DashMap<TestMatcherId, Arc<TestMatcher>>,
}

impl TestMatcherRegistry {
    pub fn new() -> Self {
        Self::default()
    }

    #[tracing::instrument(name = "TestMatcherRegistry::register", skip(self))]
    pub fn register<L>(&self, spec: L) -> TestMatcherId
    where
        L: Into<TestMatcher> + std::fmt::Debug,
    {
        let spec = Arc::new(spec.into());
        let id = TestMatcherId::next();
        self.specs.insert(id, spec);
        id
    }

    #[tracing::instrument(name = "TargetRegistry::get", skip(self))]
    pub fn get_spec(&self, id: TestMatcherId) -> Arc<TestMatcher> {
        (*self.specs.get(&id).unwrap()).clone()
    }

    pub fn len(&self) -> usize {
        self.specs.len()
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}
