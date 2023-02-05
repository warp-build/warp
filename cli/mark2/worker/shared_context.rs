use super::*;
use crate::events::EventChannel;
use crate::resolver::*;
use std::sync::Arc;

#[derive(Error, Debug)]
pub enum SharedContextError {}

#[derive(Debug, Clone)]
pub struct SharedContext {
    /*
    pub archive_manager: Arc<ArchiveManager>,

    pub artifact_store: Arc<ArtifactStore>,



    pub build_results: Arc<BuildResults>,

    pub dependency_manager: Arc<DependencyManager>,



    pub label_resolver: Arc<LabelResolver>,

    pub rule_store: Arc<RuleStore>,

    pub source_manager: Arc<SourceManager>,

    pub signature_store: Arc<SignatureStore>,

    pub target_executor: Arc<TargetExecutor>,

    /// The workspace this worker is currently executing.
    pub(crate) workspace: Workspace,
    */
    pub(crate) event_channel: Arc<EventChannel>,
    pub(crate) coordinator: Arc<Coordinator>,
    pub(crate) target_registry: Arc<TargetRegistry>,
    pub(crate) task_queue: Arc<TaskQueue>,
    pub(crate) task_results: Arc<TaskResults>,
    pub(crate) options: Options,
}

impl SharedContext {
    #[tracing::instrument(name = "SharedContext::new")]
    pub async fn new(
        // workspace: Workspace,
        event_channel: Arc<EventChannel>,
        options: Options,
    ) -> Result<Self, SharedContextError> {
        let coordinator = Arc::new(Coordinator::new());

        let target_registry = Arc::new(TargetRegistry::new());

        let task_results = Arc::new(TaskResults::new(target_registry.clone()));

        let task_queue = Arc::new(TaskQueue::new(
            task_results.clone(),
            target_registry.clone(),
            event_channel.clone(),
        ));

        /*
        let archive_manager = Arc::new(ArchiveManager::new(&workspace, event_channel.clone()));



        let toolchain_manager = Arc::new(
            ToolchainManager::new(&workspace, label_registry.clone())
                .map_err(SharedContextError::ToolchainManagerError)?,
        );

        let analyzer_service_manager = Arc::new(AnalyzerServiceManager::new(
            &workspace,
            label_registry.clone(),
            build_results.clone(),
            event_channel.clone(),
            build_opts,
        ));

        let resolver_service_manager = Arc::new(ResolverServiceManager::new(
            &workspace,
            label_registry.clone(),
            build_results.clone(),
            event_channel.clone(),
            build_opts,
        ));

        let dependency_manager = Arc::new(
            DependencyManager::new(&workspace, label_registry.clone())
                .await
                .map_err(SharedContextError::DependencyManagerError)?,
        );

        let build_queue = Arc::new(BuildQueue::new(
            build_results.clone(),
            label_registry.clone(),
            event_channel.clone(),
        ));

        let artifact_store = Arc::new(ArtifactStore::new(&workspace, event_channel.clone()));

        let rule_store = Arc::new(RuleStore::new(&workspace));

        let signature_store = Arc::new(SignatureStore::new(
            &workspace,
            build_results.clone(),
            event_channel.clone(),
            artifact_store.clone(),
            label_registry.clone(),
            analyzer_service_manager.clone(),
            dependency_manager.clone(),
            build_opts,
        ));

        let source_manager = Arc::new(SourceManager::new(
            &workspace,
            build_results.clone(),
            event_channel.clone(),
            artifact_store.clone(),
            label_registry.clone(),
            analyzer_service_manager.clone(),
            dependency_manager.clone(),
            build_opts,
        ));

        let label_resolver = Arc::new(LabelResolver::new(
            &workspace,
            artifact_store.clone(),
            build_results.clone(),
            event_channel.clone(),
            label_registry.clone(),
            dependency_manager.clone(),
            source_manager.clone(),
            signature_store.clone(),
            toolchain_manager.clone(),
            resolver_service_manager.clone(),
            build_opts,
        ));

        let target_executor = Arc::new(TargetExecutor::new(
            artifact_store.clone(),
            build_results.clone(),
            event_channel.clone(),
        ));

        Ok(Self {
            archive_manager,
            artifact_store,
            build_coordinator,
            build_opts,
            build_queue,
            build_results,
            dependency_manager,
            event_channel,
            label_registry,
            label_resolver,
            rule_store,
            signature_store,
            source_manager,
            target_executor,
            workspace,
        })
        */

        Ok(Self {
            coordinator,
            event_channel,
            target_registry,
            task_queue,
            task_results,
            options,
        })
    }
}
