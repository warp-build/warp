use super::*;
use std::{collections::BTreeMap, sync::Arc};
use thiserror::*;
use tokio::fs;
use tracing::*;

#[derive(Debug, Clone)]
pub struct Dependency {
    pub label: LabelId,
    pub version: String,
}

#[derive(Debug, Clone)]
pub struct DependencyManager {
    file: DependencyFile,
    dependencies: BTreeMap<LabelId, Dependency>,
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
            fs::write(&dep_file, "{}").await.unwrap();
        }

        let dependency_file = DependencyFile::read_from_file(&dep_file)
            .await
            .map_err(DependencyManagerError::DependencyFileError)
            .unwrap();

        let mut dependencies = BTreeMap::new();

        for (url, version) in &dependency_file.dependencies {
            let url = url::Url::parse(url).unwrap();
            let label = label_registry.register(Label::builder().from_url(&url).unwrap());
            let dep = Dependency {
                label,
                version: version.to_string(),
            };
            dependencies.insert(label, dep);
        }

        Ok(Self {
            file: dependency_file,
            dependencies,
        })
    }

    pub fn get(&self, label: LabelId) -> Option<Dependency> {
        self.dependencies.get(&label).cloned()
    }
}
