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

    pub fn from_json(dep_json: DependencyJson, label_registry: &LabelRegistry) -> Self {
        let label: Label = dep_json.url.clone().into();
        let label = label_registry.register_label(label);

        let mut resolver = None;
        for dep in &dep_json.resolver {
            let label: Label = dep.clone().parse().unwrap();
            resolver = Some(label_registry.register_label(label));
        }

        let version = dep_json.version.to_string();

        let package = dep_json.package.to_string();

        let url = dep_json.url;

        Dependency {
            label,
            resolver,
            version,
            package,
            url,
        }
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
