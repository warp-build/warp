use super::*;
use dashmap::DashMap;
use std::sync::Arc;
use thiserror::*;
use tokio::fs;
use tracing::*;

#[derive(Debug, Clone)]
pub struct DependencyManager {
    dependencies: DashMap<LabelId, Dependency>,
    label_registry: Arc<LabelRegistry>,
}

#[derive(Error, Debug)]
pub enum DependencyManagerError {
    #[error(transparent)]
    DependencyFileError(DependencyFileError),
}

impl DependencyManager {
    pub async fn new(
        workspace: &Workspace,
        label_registry: Arc<LabelRegistry>,
    ) -> Result<Self, DependencyManagerError> {
        let dep_file = workspace.paths.local_warp_root.join(DEPENDENCIES_JSON);
        if fs::metadata(&dep_file).await.is_err() {
            let dep_file = DependencyFile::builder()
                .version("0".to_string())
                .build()
                .unwrap();
            dep_file
                .write(&workspace.paths.local_warp_root)
                .await
                .unwrap();
        }

        let dependency_file = DependencyFile::read_from_file(&dep_file)
            .await
            .map_err(DependencyManagerError::DependencyFileError)
            .unwrap();

        let dependencies = DashMap::new();

        for (url, dep_json) in &dependency_file.dependencies {
            let label: Label = url::Url::parse(url).unwrap().into();
            let label = label_registry.register_label(label);

            let resolver = dep_json
                .resolver
                .clone()
                .map(|r| label_registry.register_label(r));

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
        })
    }

    pub fn get(&self, label: LabelId) -> Option<Dependency> {
        self.dependencies.get(&label).map(|r| (*r).clone())
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
