/// WARNING: He-man, stateful, multi-threaded programming happening below.
///
use super::*;
use dashmap::DashMap;
use std::collections::HashMap;
use std::sync::Arc;
use thiserror::*;
use tracing::*;

#[derive(Error, Debug)]
pub enum BuildExecutorError {
    #[error(transparent)]
    WorkerError(build_worker::WorkerError),

    #[error(transparent)]
    Unknown(anyhow::Error),
}

/// A BuildExecutor orchestrates a local build starting from the target and
/// building dependencies as needed.
///
pub struct BuildExecutor {
    /// The workspace this worker is currently executing.
    pub workspace: Workspace,

    pub results: Arc<BuildResults>,

    pub coordinator: Arc<BuildCoordinator>,

    /// The amount of workers to spawn.
    pub worker_limit: usize,
}

impl BuildExecutor {
    #[tracing::instrument(name = "BuildExecutor::from_workspace")]
    pub fn from_workspace(workspace: Workspace, worker_limit: usize) -> BuildExecutor {
        BuildExecutor {
            workspace,
            worker_limit,
            results: Arc::new(BuildResults::new()),
            coordinator: Arc::new(BuildCoordinator::new()),
        }
    }

    #[tracing::instrument(name = "BuildExecutor::build", skip(self))]
    pub async fn build(
        &self,
        target: Label,
        event_channel: Arc<EventChannel>,
    ) -> Result<Option<ComputedTarget>, anyhow::Error> {
        let build_queue = Arc::new(BuildQueue::new(
            target.clone(),
            self.results.clone(),
            event_channel.clone(),
        ));
        let toolchain_provides_map: Arc<DashMap<Label, HashMap<String, String>>> =
            Arc::new(DashMap::new());

        let worker_limit = self.worker_limit;
        debug!("Starting build executor with {} workers...", &worker_limit);

        let main_worker_span = trace_span!("BuildExecutor::main_worker").entered();

        let mut worker = BuildWorker::new(
            Role::MainWorker,
            &self.workspace,
            target.clone(),
            self.coordinator.clone(),
            self.results.clone(),
            build_queue.clone(),
            event_channel.clone(),
            toolchain_provides_map.clone(),
        );

        let main_worker_span = main_worker_span.exit();

        let mut worker_tasks = vec![];
        if worker_limit > 0 {
            let worker_pool = tokio_util::task::LocalPoolHandle::new(worker_limit);
            for worker_id in 1..worker_limit {
                let sub_worker_span = trace_span!("BuildExecutor::sub_worker");
                let build_coordinator = self.coordinator.clone();
                let build_queue = build_queue.clone();
                let build_results = self.results.clone();
                let event_channel = event_channel.clone();
                let target = target.clone();
                let toolchain_provides_map = toolchain_provides_map.clone();
                let workspace = self.workspace.clone();
                let thread = worker_pool.spawn_pinned(move || async move {
                    let mut worker = BuildWorker::new(
                        Role::HelperWorker(worker_id),
                        &workspace,
                        target,
                        build_coordinator,
                        build_results,
                        build_queue,
                        event_channel,
                        toolchain_provides_map,
                    );

                    worker
                        .load_rules()
                        .await
                        .map_err(BuildExecutorError::WorkerError)?;

                    worker
                        .setup_and_run(worker_limit)
                        .instrument(sub_worker_span)
                        .await
                        .map_err(BuildExecutorError::WorkerError)
                });
                worker_tasks.push(thread);
            }
        }

        let _span = main_worker_span.enter();
        futures::future::join(futures::future::join_all(worker_tasks), async {
            worker
                .setup_and_run(worker_limit)
                .await
                .map_err(BuildExecutorError::WorkerError)
        })
        .await
        .1?;

        Ok(self.results.get_computed_target(&target))
    }
}
