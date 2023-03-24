use super::*;
use crate::archive::ArchiveManager;
use crate::code::{CodeDatabase, CodeDatabaseError};
use crate::events::event::WorkflowEvent;

use crate::executor::local::LocalExecutor;
use crate::model::{Goal, Target, TestMatcher};
use crate::planner::DefaultPlanner;
use crate::resolver::{DefaultResolver, ResolverError, TargetRegistry};
use crate::rules::JsRuleExecutor;
use crate::store::{DefaultStore, Package, Packer, PackerError};
use crate::sync::Arc;
use crate::testing::TestMatcherRegistry;
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
    packer: Packer,
}

impl WarpDriveMarkII {
    #[tracing::instrument(name = "WarpDriveMarkII::new")]
    pub async fn new(config: Config) -> Result<Self, WarpDriveError> {
        let workspace_manager = Arc::new(WorkspaceManager::new(config.clone()));
        workspace_manager.load_current_workspace(&config).await?;

        let archive_manager = Arc::new(ArchiveManager::new(&config));
        let store: Arc<DefaultStore> =
            DefaultStore::new(config.clone(), archive_manager.clone()).into();

        let target_registry = Arc::new(TargetRegistry::new());

        let test_matcher_registry = Arc::new(TestMatcherRegistry::new());

        let task_results = Arc::new(TaskResults::new(target_registry.clone()));

        let code_db = Arc::new(CodeDatabase::new(config.clone())?);

        let resolver: DefaultResolver<GrpcTricorder> = DefaultResolver::new(
            config.clone(),
            store.clone(),
            target_registry.clone(),
            test_matcher_registry.clone(),
            archive_manager.clone(),
            workspace_manager.clone(),
            task_results.clone(),
            code_db.clone(),
        )?;

        let shared_ctx = LocalSharedContext::new(
            config.clone(),
            target_registry.clone(),
            test_matcher_registry.clone(),
            resolver,
            store,
            workspace_manager,
            task_results,
            code_db,
        );

        let worker_pool = WorkerPool::from_shared_context(config.clone(), shared_ctx.clone());

        let packer = Packer::new(
            archive_manager,
            target_registry,
            shared_ctx.task_results.clone(),
        );

        Ok(Self {
            config,
            shared_ctx,
            worker_pool,
            packer,
        })
    }

    /// Test the `targets` according to the `spec`.
    ///
    #[tracing::instrument(name = "WarpDriveMarkII::run_test", skip(self))]
    pub async fn run_test<S, T>(
        &mut self,
        spec: S,
        targets: &[T],
    ) -> Result<Arc<TaskResults>, WarpDriveError>
    where
        S: Into<TestMatcher> + std::fmt::Debug,
        T: Into<Target> + Clone + std::fmt::Debug,
    {
        let matcher_id = self.shared_ctx.test_matcher_registry.register(spec);
        let goal = Goal::Test {
            matcher_id: Some(matcher_id),
        };
        self.execute(goal, targets).await
    }

    /// Execute the `targets`.
    ///
    #[tracing::instrument(name = "WarpDriveMarkII::execute", skip(self))]
    pub async fn execute<T>(
        &mut self,
        goal: Goal,
        targets: &[T],
    ) -> Result<Arc<TaskResults>, WarpDriveError>
    where
        T: Into<Target> + Clone + std::fmt::Debug,
    {
        self.shared_ctx
            .event_channel
            .send(WorkflowEvent::BuildStarted(self.config.created_at()));

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

    /// Packs already built targets
    ///
    #[tracing::instrument(name = "WarpDriveMarkII::pack", skip(self))]
    pub async fn pack(&mut self, target: Target) -> Result<Package, WarpDriveError> {
        let result = self.packer.pack(target).await?;
        Ok(result)
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

    #[error(transparent)]
    PackerError(PackerError),

    #[error("Could not set the current directory to {root:?} due to {err:?}")]
    CouldNotSetCurrentDir {
        err: std::io::Error,
        root: std::path::PathBuf,
    },

    #[error(transparent)]
    ResolverError(ResolverError),

    #[error(transparent)]
    CodeDatabaseError(CodeDatabaseError),
}

impl From<PackerError> for WarpDriveError {
    fn from(err: PackerError) -> Self {
        Self::PackerError(err)
    }
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

impl From<ResolverError> for WarpDriveError {
    fn from(err: ResolverError) -> Self {
        WarpDriveError::ResolverError(err)
    }
}

impl From<CodeDatabaseError> for WarpDriveError {
    fn from(err: CodeDatabaseError) -> Self {
        WarpDriveError::CodeDatabaseError(err)
    }
}
