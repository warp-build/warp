use super::*;
use dashmap::DashMap;
use std::{collections::BTreeMap, sync::Arc};
use thiserror::*;
use tracing::*;

#[derive(Debug, Clone)]
pub struct DependencyManager {
    dependencies: DashMap<LabelId, Dependency>,
    label_registry: Arc<LabelRegistry>,
    dependency_file: std::path::PathBuf,
}

#[derive(Error, Debug)]
pub enum DependencyManagerError {
    #[error(transparent)]
    DependencyFileError(DependencyFileError),

    #[error(transparent)]
    DependencyJsonError(DependencyJsonBuilderError),
}

impl DependencyManager {
    pub async fn new(
        workspace: &Workspace,
        label_registry: Arc<LabelRegistry>,
    ) -> Result<Self, DependencyManagerError> {
        let dependency_file = workspace.paths.local_warp_root.join(DEPENDENCIES_JSON);
        let dep_file = DependencyFile::read_from_file(&dependency_file)
            .await
            .map_err(DependencyManagerError::DependencyFileError)
            .unwrap();

        let dependencies = DashMap::new();

        for (url, dep_json) in &dep_file.dependencies {
            let label: Label = url::Url::parse(url).unwrap().into();
            let label = label_registry.register_label(label);

            let mut resolver = None;
            for dep in &dep_json.resolver {
                let label: Label = dep.clone().parse().unwrap();
                resolver = Some(label_registry.register_label(label));
            }

            let version = dep_json.version.to_string();

            let package = dep_json.package.to_string();

            let url = dep_json.url.clone();

            let dep = Dependency {
                label,
                resolver,
                version,
                package,
                url,
            };

            dependencies.insert(label, dep);
        }

        Ok(Self {
            dependencies,
            label_registry,
            dependency_file,
        })
    }

    pub fn into_file(&self) -> Result<DependencyFile, DependencyManagerError> {
        let mut deps = BTreeMap::new();

        for dep in self.dependencies.iter() {
            let dep = &*dep;
            let dep_json = DependencyJson::builder()
                .url(dep.url.clone())
                .resolver(dep.resolver.as_ref().map(|resolver_id| {
                    self.label_registry
                        .get_label(*resolver_id)
                        .as_ref()
                        .to_string()
                }))
                .version(dep.version.clone())
                .package(dep.package.clone())
                .build()
                .map_err(DependencyManagerError::DependencyJsonError)?;

            deps.insert(dep.url.to_string(), dep_json);
        }

        let dependency_file = DependencyFile::builder()
            .dependencies(deps)
            .build()
            .map_err(DependencyManagerError::DependencyFileError)?;

        Ok(dependency_file)
    }

    pub async fn persist(&self) -> Result<(), DependencyManagerError> {
        self.into_file()?
            .write(&self.dependency_file)
            .await
            .map_err(DependencyManagerError::DependencyFileError)?;

        Ok(())
    }

    pub fn labels(&self) -> Vec<LabelId> {
        self.dependencies.iter().map(|e| *(e.key())).collect()
    }

    pub fn add(&self, dep: Dependency) {
        self.dependencies.insert(dep.label, dep);
    }

    pub fn get(&self, label: LabelId) -> Option<Dependency> {
        self.dependencies.get(&label).map(|r| (*r).clone())
    }

    pub fn find_by_package_name(&self, name: &str) -> Option<Dependency> {
        self.dependencies
            .iter()
            .find(|dep| dep.package == name)
            .map(|dep| (*dep).clone())
    }

    // TODO(@ostera): this should be part of the WorkspaceManager
    pub async fn register_workspace(
        &self,
        workspace: &Workspace,
    ) -> Result<(), DependencyManagerError> {
        let dependency_manager = Self::new(workspace, self.label_registry.clone()).await?;
        for (label_id, dep) in dependency_manager.dependencies.into_iter() {
            if self.dependencies.contains_key(&label_id) {
                continue;
            }
            self.dependencies.insert(label_id, dep);
        }
        Ok(())
    }
}
