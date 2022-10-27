use super::*;
use thiserror::*;
use tokio::fs;
use tracing::*;

#[derive(Debug, Clone, Builder)]
pub struct Dependency {
    pub label: LabelId,
    #[builder(default)]
    pub resolver: Option<LabelId>,
    pub version: String,
}

impl Dependency {
    pub fn builder() -> DependencyBuilder {
        DependencyBuilder::default()
    }
}

#[derive(Debug, Clone, Default, Builder)]
pub struct DependencySet {
    dependencies: Vec<Dependency>,
}

impl DependencySet {
    pub fn builder() -> DependencySetBuilder {
        DependencySetBuilder::default()
    }
}
