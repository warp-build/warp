use super::*;
use std::path::PathBuf;
use std::sync::Arc;
use std::time::Instant;
use thiserror::*;

#[derive(Error, Debug)]
pub enum WarpEngineError {
    #[error(transparent)]
    WorkspaceFileError(WorkspaceFileError),

    #[error("This Warp Engine has already been initialized.")]
    AlreadyInitialized,

    #[error("Can not execute a build before initializing the Warp Engine")]
    ExecuteBeforeInitialize,

    #[error("Could not set the current directory to {root:?} due to {err:?}")]
    CouldNotSetCurrentDir { root: PathBuf, err: std::io::Error },

    #[error(transparent)]
    WorkspacePathsError(WorkspacePathsError),

    #[error(transparent)]
    WorkspaceError(WorkspaceError),

    #[error(transparent)]
    BuildExecutorError(BuildExecutorError),
}

#[derive(Debug, Clone, Builder)]
pub struct WarpEngine {
    #[builder(default)]
    build_executor: Option<BuildExecutor>,

    #[builder(default)]
    pub current_user: String,

    #[builder(default)]
    pub event_channel: Arc<EventChannel>,

    pub invocation_dir: PathBuf,

    pub start_time: Instant,

    #[builder(default)]
    warp_root: Option<String>,

    #[builder(default)]
    pub workspace: Workspace,

    #[builder(default = "false")]
    initialized: bool,
}

impl WarpEngine {
    pub fn builder() -> WarpEngineBuilder {
        WarpEngineBuilder::default()
    }

    pub async fn initialize(mut self) -> Result<Self, WarpEngineError> {
        self.event_channel
            .send(Event::BuildStarted(self.start_time));

        if self.initialized {
            return Err(WarpEngineError::AlreadyInitialized);
        }

        let (root, workspace_file) = WorkspaceFile::find_upwards(&self.invocation_dir)
            .await
            .map_err(WarpEngineError::WorkspaceFileError)?;

        std::env::set_current_dir(&root).map_err(|err| WarpEngineError::CouldNotSetCurrentDir {
            err,
            root: root.to_path_buf(),
        })?;

        let paths = WorkspacePaths::new(&root, self.warp_root.clone(), self.current_user.clone())
            .map_err(WarpEngineError::WorkspacePathsError)?;

        self.workspace = Workspace::builder()
            .current_user(self.current_user.clone())
            .paths(paths)
            .from_file(workspace_file)
            .await
            .unwrap()
            .build()
            .map_err(WarpEngineError::WorkspaceError)?;

        self.initialized = true;

        Ok(self)
    }

    pub async fn shutdown(self) -> Result<(), WarpEngineError> {
        std::env::set_current_dir(&self.invocation_dir).map_err(|err| {
            WarpEngineError::CouldNotSetCurrentDir {
                err,
                root: self.invocation_dir.clone(),
            }
        })?;
        Ok(())
    }

    pub async fn prepare_executor(&mut self, build_opts: BuildOpts) -> Result<(), WarpEngineError> {
        if !self.initialized {
            return Err(WarpEngineError::ExecuteBeforeInitialize);
        }

        self.build_executor = Some(
            BuildExecutor::new(
                self.workspace.clone(),
                self.event_channel.clone(),
                build_opts,
            )
            .await
            .map_err(WarpEngineError::BuildExecutorError)?,
        );

        Ok(())
    }

    pub async fn execute(
        &mut self,
        labels: &[Label],
        build_opts: BuildOpts,
    ) -> Result<(), WarpEngineError> {
        self.prepare_executor(build_opts).await?;

        self.build_executor
            .as_ref()
            .unwrap()
            .execute(labels)
            .await
            .map_err(WarpEngineError::BuildExecutorError)
    }

    pub fn prepare_for_new_run(&self) {
        self.build_executor.as_ref().unwrap().clear_results()
    }

    pub fn get_results(&self) -> Vec<BuildResult> {
        self.build_executor.as_ref().unwrap().get_results()
    }

    pub fn label_registry(&self) -> Arc<LabelRegistry> {
        self.build_executor.as_ref().unwrap().label_registry.clone()
    }

    pub fn archive_manager(&self) -> Arc<ArchiveManager> {
        self.build_executor
            .as_ref()
            .unwrap()
            .archive_manager
            .clone()
    }

    pub fn dependency_manager(&self) -> Arc<DependencyManager> {
        self.build_executor
            .as_ref()
            .unwrap()
            .dependency_manager
            .clone()
    }

    pub fn source_manager(&self) -> Arc<SourceManager> {
        self.build_executor.as_ref().unwrap().source_manager.clone()
    }

    pub fn signature_store(&self) -> Arc<SignatureStore> {
        self.build_executor
            .as_ref()
            .unwrap()
            .signature_store
            .clone()
    }
}
