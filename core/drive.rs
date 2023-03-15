use super::*;
use crate::archive::ArchiveManager;
use crate::events::EventChannel;
use crate::executor::local::LocalExecutor;
use crate::model::{Goal, Target};
use crate::planner::DefaultPlanner;
use crate::resolver::{DefaultResolver, TargetRegistry};
use crate::rules::JsRuleExecutor;
use crate::store::DefaultStore;
use crate::sync::Arc;
use crate::tricorder::GrpcTricorder;
use crate::worker::local::{LocalSharedContext, LocalWorker};
use crate::worker::{Task, TaskResults, WorkerPool, WorkerPoolError};
use crate::workspace::{WorkspaceManager, WorkspaceManagerError};
use thiserror::*;
use tracing::*;

type MainResolver = DefaultResolver<GrpcTricorder>;
type MainPlanner = DefaultPlanner<JsRuleExecutor>;
type DefaultWorker = LocalWorker<MainResolver, MainPlanner, LocalExecutor, DefaultStore>;
type DefaultCtxt = LocalSharedContext<MainResolver, DefaultStore>;

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
pub struct WarpDriveMarkII {
    worker_pool: WorkerPool<DefaultWorker>,
    shared_ctx: DefaultCtxt,
    config: Config,
}

impl WarpDriveMarkII {
    #[tracing::instrument(name = "WarpDriveMarkII::new")]
    pub async fn new(config: Config) -> Result<Self, WarpDriveError> {
        let event_channel = Arc::new(EventChannel::new());

        let workspace_manager = Arc::new(WorkspaceManager::new(config.clone()));
        workspace_manager.load_current_workspace(&config).await?;

        let archive_manager = Arc::new(ArchiveManager::new(&config));
        let store: Arc<DefaultStore> =
            DefaultStore::new(config.clone(), archive_manager.clone()).into();

        let target_registry = Arc::new(TargetRegistry::new());
        let resolver: DefaultResolver<GrpcTricorder> = DefaultResolver::new(
            config.clone(),
            store.clone(),
            target_registry.clone(),
            archive_manager,
            workspace_manager.clone(),
        );

        let shared_ctx = LocalSharedContext::new(
            event_channel.clone(),
            config.clone(),
            target_registry,
            resolver,
            store,
            workspace_manager,
        );

        let worker_pool = WorkerPool::from_shared_context(
            event_channel.clone(),
            config.clone(),
            shared_ctx.clone(),
        );

        Ok(Self {
            config,
            shared_ctx,
            worker_pool,
        })
    }

    /// Execute the `targets`.
    ///
    #[tracing::instrument(name = "WarpDriveMarkII::execute", skip(self))]
    pub async fn execute(
        &mut self,
        goal: Goal,
        targets: &[Target],
    ) -> Result<Arc<TaskResults>, WarpDriveError> {
        self.shared_ctx
            .event_channel
            .send(events::Event::BuildStarted(self.config.created_at()));

        self.go_to_workspace_root()?;

        let target_ids = self
            .shared_ctx
            .target_registry
            .register_many_targets(targets);

        let tasks: Vec<Task> = target_ids
            .into_iter()
            .map(|target_id| Task::new(goal, target_id))
            .collect();

        let results = self.worker_pool.execute(&tasks).await?;

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
        std::env::set_current_dir(self.config.invocation_dir()).map_err(|err| {
            WarpDriveError::CouldNotSetCurrentDir {
                err,
                root: self.config.invocation_dir().into(),
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
