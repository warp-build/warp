use super::package::Package;
use super::{ArtifactManifest, PackageManifest};
use crate::archive::{ArchiveManager, ArchiveManagerError};

use crate::resolver::TargetRegistry;
use crate::sync::Arc;
use crate::worker::TaskResults;
use crate::Target;
use thiserror::Error;

pub struct Packer {
    archive_manager: Arc<ArchiveManager>,
    task_results: Arc<TaskResults>,
    target_registry: Arc<TargetRegistry>,
}

impl Packer {
    pub fn new(
        archive_manager: Arc<ArchiveManager>,
        target_registry: Arc<TargetRegistry>,
        task_results: Arc<TaskResults>,
    ) -> Self {
        Self {
            archive_manager,
            task_results,
            target_registry,
        }
    }

    pub async fn pack(&self, target: Target) -> Result<Package, PackerError> {
        let target_id = self.target_registry.find_target(&target).unwrap();
        let task = self.task_results.find_task_by_target_id(target_id).unwrap();
        let task_result = self.task_results.get_task_result(&task).unwrap();
        let manifest = task_result.artifact_manifest;

        let included_deps: Vec<String> = manifest
            .deps()
            .values()
            .chain(manifest.runtime_deps().values())
            .chain(manifest.transitive_deps().values())
            .chain(manifest.toolchains().values())
            .cloned()
            .collect();

        let deps: Vec<Arc<ArtifactManifest>> = self
            .task_results
            .get_results()
            .iter()
            .filter_map(|result| {
                let dep_hash = result.artifact_manifest.hash();
                if included_deps.contains(&dep_hash.to_string()) {
                    Some(result.artifact_manifest.clone())
                } else {
                    None
                }
            })
            .collect();

        let main_archive = self.archive_manager.compress(manifest.clone()).await?;
        let manifest = PackageManifest::from_artifact_manifest(manifest);
        let mut dep_archives = vec![];

        {
            let mut compress_results = vec![];
            for dep in deps {
                compress_results.push(self.archive_manager.compress(dep));
            }

            let compress_results = futures::future::join_all(compress_results).await;

            for result in compress_results {
                dep_archives.push(result?);
            }
        }

        let pkg = Package::builder()
            .main(main_archive)
            .manifest(manifest)
            .deps(dep_archives)
            .build()
            .unwrap();

        Ok(pkg)
    }
}

#[derive(Error, Debug)]
pub enum PackerError {
    #[error(transparent)]
    CompressError(ArchiveManagerError),
}

impl From<ArchiveManagerError> for PackerError {
    fn from(value: ArchiveManagerError) -> Self {
        PackerError::CompressError(value)
    }
}
