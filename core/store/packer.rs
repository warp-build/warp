use super::package::Package;
use super::PackageManifest;
use crate::archive::{Archive, ArchiveManager, ArchiveManagerError};

use crate::events::event::PackerEvent;
use crate::events::EventChannel;
use crate::resolver::TargetRegistry;
use crate::sync::Arc;
use crate::worker::{TaskResult, TaskResults};
use crate::Target;
use thiserror::Error;

pub struct Packer {
    event_channel: Arc<EventChannel>,
    archive_manager: Arc<ArchiveManager>,
    task_results: Arc<TaskResults>,
    target_registry: Arc<TargetRegistry>,
}

impl Packer {
    pub fn new(
        archive_manager: Arc<ArchiveManager>,
        target_registry: Arc<TargetRegistry>,
        task_results: Arc<TaskResults>,
        event_channel: Arc<EventChannel>,
    ) -> Self {
        Self {
            archive_manager,
            event_channel,
            task_results,
            target_registry,
        }
    }

    pub async fn pack(&self, target: Target) -> Result<Package, PackerError> {
        self.event_channel.send(PackerEvent::PackagingStarted {
            target: target.clone(),
        });

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

        let deps: Vec<TaskResult> = self
            .task_results
            .get_results()
            .into_iter()
            .filter(|result| {
                let dep_hash = result.artifact_manifest.hash();
                included_deps.contains(&dep_hash.to_string())
            })
            .collect();

        let main_archive = self.archive_manager.compress(manifest.clone()).await?;
        let manifest = PackageManifest::from_artifact_manifest(manifest);
        let mut dep_archives = vec![];

        {
            let mut compress_results = vec![];
            for dep in deps {
                let target = (*dep.executable_spec.target().original_target()).clone();
                compress_results.push(async {
                    self.event_channel.send(PackerEvent::PackagingStarted {
                        target: target.clone(),
                    });
                    let archive = self.archive_manager.compress(dep.artifact_manifest).await?;
                    self.event_channel
                        .send(PackerEvent::PackagingCompleted { target });
                    Ok(archive)
                });
            }

            let compress_results: Vec<Result<Archive, ArchiveManagerError>> =
                futures::future::join_all(compress_results).await;

            for result in compress_results {
                let archive = result?;
                dep_archives.push(archive);
            }
        }

        let pkg = Package::builder()
            .main(main_archive)
            .manifest(manifest)
            .deps(dep_archives)
            .build()
            .unwrap();

        self.event_channel.send(PackerEvent::PackagingCompleted {
            target: target.clone(),
        });

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
