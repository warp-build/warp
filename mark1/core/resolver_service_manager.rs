use super::*;
use dashmap::DashMap;
use std::{path::PathBuf, sync::Arc};
use thiserror::*;
use tokio::process::Child;

pub type ResolverServiceClient =
    proto::build::warp::dependency::resolver_service_client::ResolverServiceClient<
        tonic::transport::Channel,
    >;

pub type FileExtension = String;

#[derive(Default, Debug, Clone)]
pub struct ResolverServiceManager {
    analyzers_by_extension: DashMap<FileExtension, LabelId>,
    event_channel: Arc<EventChannel>,
    build_results: Arc<BuildResults>,
    clients: DashMap<LabelId, ResolverServiceClient>,
    label_registry: Arc<LabelRegistry>,
    processes: Arc<DashMap<LabelId, Child>>,
    build_opts: BuildOpts,

    // NOTE(@ostera): only used to serialize the calls to `start` and prevent starting the same
    // analyzer twice.
    _start_lock: Arc<tokio::sync::Mutex<()>>,
}

#[derive(Error, Debug)]
pub enum ResolverServiceManagerError {
    #[error("Could not spawn service {} due to {err:?}", label.to_string())]
    ServiceSpawnError { label: Label, err: std::io::Error },

    #[error("Need to build the service with label id: {0} before we can start it")]
    MissingService(LabelId),

    #[error(transparent)]
    CommandRunnerError(CommandRunnerError),
}

impl From<CommandRunnerError> for ResolverServiceManagerError {
    fn from(err: CommandRunnerError) -> Self {
        Self::CommandRunnerError(err)
    }
}

impl ResolverServiceManager {
    pub fn new(
        workspace: &Workspace,
        label_registry: Arc<LabelRegistry>,
        build_results: Arc<BuildResults>,
        event_channel: Arc<EventChannel>,
        build_opts: BuildOpts,
    ) -> Self {
        let analyzers_by_extension: DashMap<FileExtension, LabelId> = DashMap::new();
        for toolchain in workspace.toolchain_configs.values() {
            if let Ok(analyzer) = toolchain.get_string("analyzer") {
                let mut analyzer: Label = analyzer.parse().unwrap();
                analyzer.set_workspace(&workspace.paths.workspace_root);

                let label_id = label_registry.register_label(analyzer);
                for ext in toolchain.get_string_list("extensions").unwrap() {
                    analyzers_by_extension.insert(ext, label_id);
                }
            }
        }

        Self {
            processes: Arc::new(DashMap::new()),
            clients: DashMap::new(),
            build_results,
            label_registry,
            analyzers_by_extension,
            event_channel,
            build_opts,
            _start_lock: Arc::new(tokio::sync::Mutex::new(())),
        }
    }

    pub fn all_clients(&self) -> Vec<ResolverServiceClient> {
        self.clients.iter().map(|e| (*e).clone()).collect()
    }

    pub async fn start_all_clients(
        &self,
    ) -> Result<Vec<ResolverServiceClient>, ResolverServiceManagerError> {
        for analyzer_id in self.analyzers_by_extension.iter() {
            self.start(*analyzer_id).await?;
        }
        Ok(self.all_clients())
    }

    pub fn find_analyzer_for_local_label(&self, local_label: &LocalLabel) -> Option<LabelId> {
        let ext = local_label.extension();
        self.analyzers_by_extension.get(&*ext).map(|r| (*r))
    }

    pub async fn start(
        &self,
        label_id: LabelId,
    ) -> Result<ResolverServiceClient, ResolverServiceManagerError> {
        let _lock = self._start_lock.lock().await;

        // 1. If the process has already been started, we'll just return its handler.
        if let Some(client) = self.clients.get(&label_id) {
            return Ok((*client).to_owned());
        }

        // 2. Make sure we have already built this analyzer so we can start it
        let Some(build_result) = self.build_results.get_build_result(label_id) else {
            return Err(ResolverServiceManagerError::MissingService(label_id));
        };

        // 3. Find out the next available port
        let port = PortFinder::next().unwrap();

        // 4. Spin up the process
        let process = CommandRunner::builder()
            .cwd(PathBuf::from("."))
            .manifest(build_result.target_manifest)
            .target(build_result.executable_target)
            .stream_outputs(self.build_opts.experimental_stream_analyzer_outputs)
            .sandboxed(false)
            .args(vec!["start".into(), port.to_string()])
            .build()?
            .spawn()?;
        self.processes.insert(label_id, process);

        let conn_str = format!("http://0.0.0.0:{}", port);
        let client = loop {
            let conn = proto::build::warp::dependency::resolver_service_client::ResolverServiceClient::connect(
                conn_str.clone(),
            )
            .await;
            if let Ok(conn) = conn {
                break conn;
            }
            tokio::time::sleep(std::time::Duration::from_millis(1)).await;
        };

        self.event_channel.send(Event::StartedService {
            label: self.label_registry.get_label(label_id).as_ref().to_owned(),
        });

        // 5. Save the client to this service
        self.clients.insert(label_id, client.clone());

        Ok(client)
    }
}
