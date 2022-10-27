use super::*;
use std::{collections::BTreeMap, sync::Arc};
use thiserror::*;
use tokio::fs;
use tracing::*;

#[derive(Debug, Clone)]
pub struct DependencyManager {
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

        let mut dependencies = BTreeMap::new();

        for (url, dep_json) in &dependency_file.dependencies {
            let url = url::Url::parse(url).unwrap();
            let label = label_registry.register(Label::builder().from_url(&url).unwrap());

            let resolver = dep_json
                .resolver
                .clone()
                .map(|r| label_registry.register(r));

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

        Ok(Self { dependencies })
    }

    pub fn get(&self, label: LabelId) -> Option<Dependency> {
        self.dependencies.get(&label).cloned()
    }
}
