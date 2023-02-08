use std::sync::Arc;

use super::*;
use crate::events::EventChannel;
use crate::resolver::Target;
use crate::worker::{TaskResults, WorkerPool, WorkerPoolError};
use crate::workspace::Workspace;
use thiserror::*;

#[derive(Error, Debug)]
pub enum WarpDriveError {
    #[error(transparent)]
    WorkerPoolError(WorkerPoolError),
}

/// The MarkII of the Warp Engine starts with this struct.
///
/// Here we find our `Workspace`, initialize our shared state for our `WorkerPool`, and eventually
/// queue up `Target`s to be built.
///
/// All results are made available via a shared reference into the `TaskResults` of the
/// `WorkerPool`.
///
#[derive(Debug, Clone)]
pub struct WarpDriveMarkII {
    event_channel: Arc<EventChannel>,
    worker_pool: WorkerPool,
    opts: WarpOptions,
    workspace: Workspace,
}

impl WarpDriveMarkII {
    pub async fn new(opts: WarpOptions) -> Result<Self, WarpDriveError> {
        let event_channel = Arc::new(EventChannel::new());

        let workspace = workspace::WorkspaceFinder::find(opts).await?;

        let ctx = worker::SharedContext::new(event_channel.clone(), opts).await?;
        let worker_pool = WorkerPool::from_shared_context(ctx);

        Ok(Self {
            event_channel,
            opts,
            worker_pool,
            workspace,
        })
    }

    /// Execute the `targets`.
    ///
    pub async fn execute(
        &mut self,
        targets: &[Target],
    ) -> Result<Arc<TaskResults>, WarpDriveError> {
        self.event_channel
            .send(events::Event::BuildStarted(self.start_time));

        self.go_to_workspace_root()?;

        let results = self
            .worker_pool
            .execute(targets)
            .await
            .map_err(WarpDriveError::WorkerPoolError)?;

        self.return_to_invocation_dir()?;

        Ok(results)
    }

    fn go_to_workspace_root(&self) -> Result<(), WarpDriveError> {
        std::env::set_current_dir(&self.workspace.paths.workspace_root).map_err(|err| {
            WarpDriveError::CouldNotSetCurrentDir {
                err,
                root: self.workspace.paths.workspace_root.to_path_buf(),
            }
        })
    }

    fn return_to_invocation_dir(&self) -> Result<(), WarpDriveError> {
        std::env::set_current_dir(&self.invocation_dir).map_err(|err| {
            WarpDriveError::CouldNotSetCurrentDir {
                err,
                root: self.opts.invocation_dir.clone(),
            }
        })
    }
}
