use super::*;
use crate::archive::ArchiveManager;
use crate::code::{CodeManager, CodeManagerError};
use crate::events::event::WorkflowEvent;
use crate::executor::local::LocalExecutor;
use crate::model::{Goal, Target, TargetError, Task, TestMatcher, UnregisteredTask};
use crate::planner::DefaultPlanner;
use crate::resolver::{DefaultResolver, ResolverError, SignatureRegistry, TargetRegistry};
use crate::rules::JsRuleExecutor;
use crate::store::{DefaultStore, Package, Packer, PackerError, UpdateManifestError};
use crate::sync::Arc;
use crate::testing::TestMatcherRegistry;
use crate::tricorder::GrpcTricorder;
use crate::worker::local::{LocalSharedContext, LocalWorker};
use crate::worker::{TaskRegistry, TaskResults, WorkerPool, WorkerPoolError};
use crate::workspace::{WorkspaceManager, WorkspaceManagerError};

use thiserror::*;
use tracing::{instrument, *};

type MainResolver = DefaultResolver<DefaultStore, GrpcTricorder>;
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

pub struct WarpDriveMarkII {
    worker_pool: WorkerPool<DefaultWorker>,
    shared_ctx: DefaultCtxt,
    config: Config,
    packer: Packer,
}

impl WarpDriveMarkII {
    #[instrument(name = "WarpDriveMarkII::new")]
    pub async fn new(mut config: Config) -> Result<Self, WarpDriveError> {
        let workspace_manager = Arc::new(WorkspaceManager::new(config.clone()));
        let _wid = workspace_manager.load_current_workspace().await?;

        config.set_workspace_root(workspace_manager.current_workspace().root());

        let archive_manager = Arc::new(ArchiveManager::new(&config));
        let store: Arc<DefaultStore> =
            DefaultStore::new(config.clone(), archive_manager.clone()).into();

        let task_registry = Arc::new(TaskRegistry::new());
        let target_registry = Arc::new(TargetRegistry::new());
        let signature_registry = Arc::new(SignatureRegistry::new());

        let test_matcher_registry = Arc::new(TestMatcherRegistry::new());

        let task_results = Arc::new(TaskResults::new(
            task_registry.clone(),
            target_registry.clone(),
            signature_registry.clone(),
        ));

        let code_manager: Arc<CodeManager<DefaultStore, GrpcTricorder>> =
            Arc::new(CodeManager::new(
                config.clone(),
                store.clone(),
                test_matcher_registry.clone(),
                target_registry.clone(),
                task_registry.clone(),
                task_results.clone(),
            )?);

        let resolver: DefaultResolver<DefaultStore, GrpcTricorder> = DefaultResolver::new(
            config.clone(),
            store.clone(),
            target_registry.clone(),
            task_registry.clone(),
            signature_registry.clone(),
            test_matcher_registry.clone(),
            archive_manager.clone(),
            workspace_manager.clone(),
            task_results.clone(),
            code_manager.clone(),
        )?;

        let shared_ctx = LocalSharedContext::new(
            config.clone(),
            task_registry,
            target_registry.clone(),
            signature_registry,
            test_matcher_registry,
            resolver,
            store,
            workspace_manager,
            task_results,
            code_manager,
        );

        let worker_pool = WorkerPool::from_shared_context(config.clone(), shared_ctx.clone());

        let packer = Packer::new(
            archive_manager,
            target_registry,
            shared_ctx.task_results.clone(),
            shared_ctx.event_channel.clone(),
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
    #[instrument(name = "WarpDriveMarkII::run_test", skip(self))]
    pub async fn run_test<M, T>(
        &mut self,
        matcher: M,
        targets: &[T],
    ) -> Result<Arc<TaskResults>, WarpDriveError>
    where
        M: Into<TestMatcher> + std::fmt::Debug,
        T: Into<Target> + Clone + std::fmt::Debug,
    {
        let matcher: TestMatcher = matcher.into();
        let matcher_id = if matcher.is_all() {
            None
        } else {
            Some(self.shared_ctx.test_matcher_registry.register(matcher))
        };

        let goal = Goal::Test { matcher_id };
        self.execute(goal, targets).await
    }

    /// Execute the `targets`.
    ///
    #[instrument(name = "WarpDriveMarkII::execute", skip(self))]
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

        let targets = targets
            .iter()
            .cloned()
            .map(|t| t.into().normalize(self.config.workspace_root()))
            .collect::<Result<Vec<Target>, TargetError>>()?;

        let target_ids = self
            .shared_ctx
            .target_registry
            .register_many_targets(&targets);

        let tasks: Vec<Task> = target_ids
            .into_iter()
            .map(|target_id| {
                UnregisteredTask::builder()
                    .goal(goal)
                    .target_id(target_id)
                    .build()
                    .unwrap()
            })
            .map(|unreg_task| self.shared_ctx.task_registry.register(unreg_task))
            .collect();

        let results = self.worker_pool.execute(&tasks).await?;

        self.return_to_invocation_dir()?;

        Ok(results)
    }

    /// Packs already built targets
    ///
    #[instrument(name = "WarpDriveMarkII::pack", skip(self))]
    pub async fn pack(&mut self, target: Target, upload: bool) -> Result<Package, WarpDriveError> {
        self.go_to_workspace_root()?;

        let target = target.normalize(self.config.workspace_root())?;
        let package = self.packer.pack(target).await?;

        package
            .update_manifest_file(&self.config.workspace_root().join("store"))
            .await?;

        if upload {
            self.packer.upload_to_public_store(&package).await?;
        }

        self.return_to_invocation_dir()?;

        Ok(package)
    }

    #[instrument(name = "WarpDriveMarkII::fetch", skip(self))]
    pub async fn fetch_from_public_store(&mut self, target: Target) -> Result<(), WarpDriveError> {
        // access the Store to download files from public store

        Ok(())
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
    CodeManagerError(CodeManagerError),

    #[error(transparent)]
    TargetError(TargetError),

    #[error(transparent)]
    UpdateManifestError(UpdateManifestError),
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

impl From<CodeManagerError> for WarpDriveError {
    fn from(err: CodeManagerError) -> Self {
        WarpDriveError::CodeManagerError(err)
    }
}

impl From<TargetError> for WarpDriveError {
    fn from(err: TargetError) -> Self {
        WarpDriveError::TargetError(err)
    }
}

impl From<UpdateManifestError> for WarpDriveError {
    fn from(err: UpdateManifestError) -> Self {
        WarpDriveError::UpdateManifestError(err)
    }
}
