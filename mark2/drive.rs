use super::*;
use crate::events::EventChannel;
use crate::resolver::{DefaultResolver, Target};
use crate::sync::Arc;
use crate::worker::{LocalSharedContext, LocalWorker, TaskResults, WorkerPool, WorkerPoolError};
use crate::workspace::WorkspaceManagerError;
use thiserror::*;
use tracing::*;

/// # Warp Engine Mark II
///
/// This struct orchestrates the top-level flow of Warp.
///
/// Here we find our `Workspace`, initialize our shared state for our `WorkerPool`, and eventually
/// queue up `Target`s to be built.
///
/// All results are made available via a shared reference into the `TaskResults` of the
/// `WorkerPool`.
///
#[derive(Debug)]
pub struct WarpDriveMarkII {
    worker_pool: WorkerPool<LocalWorker<DefaultResolver>>,
    shared_ctx: LocalSharedContext<DefaultResolver>,
    opts: Config,
}

impl WarpDriveMarkII {
    #[tracing::instrument(name = "WarpDriveMarkII::new")]
    pub async fn new(opts: Config) -> Result<Self, WarpDriveError> {
        let event_channel = Arc::new(EventChannel::new());

        let shared_ctx =
            LocalSharedContext::new(event_channel.clone(), opts.clone(), DefaultResolver::new());

        let worker_pool = WorkerPool::from_shared_context(
            event_channel.clone(),
            opts.clone(),
            shared_ctx.clone(),
        );

        shared_ctx
            .workspace_manager
            .load_current_workspace(&opts)
            .await?;

        Ok(Self {
            opts,
            shared_ctx,
            worker_pool,
        })
    }

    /// Execute the `targets`.
    ///
    #[tracing::instrument(name = "WarpDriveMarkII::execute")]
    pub async fn execute(
        &mut self,
        targets: &[Target],
    ) -> Result<Arc<TaskResults>, WarpDriveError> {
        self.shared_ctx
            .event_channel
            .send(events::Event::BuildStarted(self.opts.created_at()));

        self.go_to_workspace_root()?;

        let target_ids = self
            .shared_ctx
            .target_registry
            .register_many_targets(targets);
        let results = self.worker_pool.execute(&target_ids).await?;

        self.return_to_invocation_dir()?;

        Ok(results)
    }

    fn go_to_workspace_root(&self) -> Result<(), WarpDriveError> {
        let current_workspace = self.shared_ctx.workspace_manager.current_workspace();
        let workspace_root = current_workspace.root();

        std::env::set_current_dir(workspace_root).map_err(|err| {
            WarpDriveError::CouldNotSetCurrentDir {
                err,
                root: workspace_root.to_path_buf(),
            }
        })
    }

    fn return_to_invocation_dir(&self) -> Result<(), WarpDriveError> {
        std::env::set_current_dir(self.opts.invocation_dir()).map_err(|err| {
            WarpDriveError::CouldNotSetCurrentDir {
                err,
                root: self.opts.invocation_dir().into(),
            }
        })
    }
}

#[derive(Error, Debug)]
pub enum WarpDriveError {
    #[error(transparent)]
    WorkerPoolError(WorkerPoolError),

    #[error(transparent)]
    WorkspaceManagerError(WorkspaceManagerError),

    #[error("Could not set the current directory to {root:?} due to {err:?}")]
    CouldNotSetCurrentDir {
        err: std::io::Error,
        root: std::path::PathBuf,
    },
}

impl From<WorkspaceManagerError> for WarpDriveError {
    fn from(err: WorkspaceManagerError) -> Self {
        Self::WorkspaceManagerError(err)
    }
}

impl From<WorkerPoolError> for WarpDriveError {
    fn from(err: WorkerPoolError) -> Self {
        Self::WorkerPoolError(err)
    }
}
