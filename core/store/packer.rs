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
        self.event_channel.send(PackerEvent::PackagingCompleted {
            target: target.clone(),
        });

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

        Ok(pkg)
    }

    pub async fn upload_to_public_store(&self, package: &Package) -> Result<(), PackerError> {
        let config = aws_config::load_from_env().await;
        let client = aws_sdk_s3::Client::new(&config);

        let mut calls = vec![];

        let mut archives = package.deps().to_vec();
        archives.push(package.main().clone());

        for archive in archives {
            let ec = self.event_channel.clone();
            let client = client.clone();
            let url = archive.url().to_owned();
            let bucket = "arn:aws:s3:eu-north-1:160208198511:accesspoint/local";
            calls.push(async move {
                let key = url.path_segments().unwrap().next().unwrap();

                let result = client.head_object().bucket(bucket).key(key).send().await;

                if result.is_ok() {
                    ec.send(PackerEvent::UploadSkipped { url });
                    return Ok(());
                }

                ec.send(PackerEvent::UploadStarted { url: url.clone() });

                let body = aws_sdk_s3::types::ByteStream::from_path(archive.final_path())
                    .await
                    .unwrap();

                let _result = client
                    .put_object()
                    .bucket(bucket)
                    .key(key)
                    .acl(aws_sdk_s3::model::ObjectCannedAcl::PublicRead)
                    .body(body)
                    .send()
                    .await?;

                ec.send(PackerEvent::UploadCompleted { url });

                Ok(())
            });
        }

        let calls: Vec<Result<(), PackerError>> = futures::future::join_all(calls).await;

        for call in calls {
            call?;
        }

        Ok(())
    }
}

#[derive(Error, Debug)]
pub enum PackerError {
    #[error(transparent)]
    CompressError(ArchiveManagerError),

    #[error(transparent)]
    UploadError(aws_sdk_s3::types::SdkError<aws_sdk_s3::error::PutObjectError>),

    #[error(transparent)]
    StoreCheckError(aws_sdk_s3::types::SdkError<aws_sdk_s3::error::HeadObjectError>),
}

impl From<ArchiveManagerError> for PackerError {
    fn from(value: ArchiveManagerError) -> Self {
        PackerError::CompressError(value)
    }
}

impl From<aws_sdk_s3::types::SdkError<aws_sdk_s3::error::PutObjectError>> for PackerError {
    fn from(value: aws_sdk_s3::types::SdkError<aws_sdk_s3::error::PutObjectError>) -> Self {
        PackerError::UploadError(value)
    }
}

impl From<aws_sdk_s3::types::SdkError<aws_sdk_s3::error::HeadObjectError>> for PackerError {
    fn from(value: aws_sdk_s3::types::SdkError<aws_sdk_s3::error::HeadObjectError>) -> Self {
        PackerError::StoreCheckError(value)
    }
}
