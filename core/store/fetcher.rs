use std::sync::Arc;

use thiserror::Error;

use crate::archive::{ArchiveManager, ArchiveManagerError};
use crate::events::event::FetcherEvent;
use crate::events::EventChannel;
use crate::resolver::TargetRegistry;
use crate::worker::TaskResults;
use crate::Target;

pub struct Fetcher {
    archive_manager: Arc<ArchiveManager>,
    target_registry: Arc<TargetRegistry>,
    task_results: Arc<TaskResults>,
    event_channel: Arc<EventChannel>,
}

impl Fetcher {
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

    pub async fn download(&self, target: Target) -> Result<(), FetcherError> {
        self.event_channel.send(FetcherEvent::FetchingStarted {
            target: target.clone(),
        });

        let Some(target) = target.as_remote() else {
            self.event_channel.send(FetcherEvent::FetchingSkipped {
                target: target.clone(),
            });
            return Ok(());
        };

        self.archive_manager.download(&target.url()).await?;

        Ok(())
    }
}

#[derive(Error, Debug)]
pub enum FetcherError {
    #[error(transparent)]
    ArchiveManagerError(ArchiveManagerError),
}

impl From<ArchiveManagerError> for FetcherError {
    fn from(value: ArchiveManagerError) -> Self {
        FetcherError::ArchiveManagerError(value)
    }
}
