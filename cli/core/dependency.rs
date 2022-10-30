use super::*;

#[derive(Debug, Clone, Builder)]
pub struct Dependency {
    pub label: LabelId,

    #[builder(default)]
    pub resolver: Option<LabelId>,

    pub package: String,

    pub version: String,

    pub url: url::Url,
}

impl Dependency {
    pub fn builder() -> DependencyBuilder {
        DependencyBuilder::default()
    }
}

#[derive(Debug, Clone, Default, Builder)]
pub struct DependencySet {
    pub dependencies: Vec<Dependency>,
}

impl DependencySet {
    pub fn builder() -> DependencySetBuilder {
        DependencySetBuilder::default()
    }
}
