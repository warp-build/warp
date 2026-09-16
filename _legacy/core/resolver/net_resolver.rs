use std::path::PathBuf;

use super::{ResolverError, TargetRegistry};
use crate::archive::ArchiveManager;
use crate::code::CodeManager;
use crate::model::{ConcreteTarget, RemoteTarget, Task};
use crate::store::Store;
use crate::sync::Arc;
use crate::tricorder::{SignatureGenerationFlow, Tricorder};
use crate::workspace::WorkspaceManager;
use tokio::fs;
use tracing::instrument;

#[cfg_attr(doc, aquamarine::aquamarine)]
/// Network Resolver for Remote Targets.
///
/// The [NetResolver] takes cares of downloading, unpacking, and readying network dependencies. To
/// do this it relis on the [ArchiveManager], the [WorkspaceManager], and the [TricorderManager].
///
/// It's flow goes:
///
/// ```mermaid
/// graph TD
///   RemoteTarget
///   --> |resolve| NetResolver
///   --> |register workspace| WorkspaceManager
///
///   WorkspaceManager --> |workspace root| ArchiveManager
///   NetResolver --> |download| ArchiveManager
///
///   NetResolver -->|find and ready| TricorderManager
///   TricorderManager --> |ready dependency| Tricorder
///   ArchiveManager --> |file inputs| Tricorder
///   Tricorder --> |generates| Signature
/// ```
#[derive(Clone)]
pub struct NetResolver<S: Store, T: Tricorder> {
    archive_manager: Arc<ArchiveManager>,
    workspace_manager: Arc<WorkspaceManager>,
    target_registry: Arc<TargetRegistry>,
    code_manager: Arc<CodeManager<S, T>>,
}

impl<S, T> NetResolver<S, T>
where
    S: Store,
    T: Tricorder + Clone + 'static,
{
    pub fn new(
        archive_manager: Arc<ArchiveManager>,
        workspace_manager: Arc<WorkspaceManager>,
        target_registry: Arc<TargetRegistry>,
        code_manager: Arc<CodeManager<S, T>>,
    ) -> Self {
        Self {
            archive_manager,
            workspace_manager,
            target_registry,
            code_manager,
        }
    }

    #[instrument(name = "NetResolver::resolve", skip(self))]
    pub async fn resolve(
        &self,
        task: Task,
        target: &RemoteTarget,
    ) -> Result<SignatureGenerationFlow, ResolverError> {
        let workspace_id = self
            .workspace_manager
            .register_remote_workspace(&target.url());
        let workspace = self.workspace_manager.get_workspace(workspace_id);

        let ct = ConcreteTarget::builder()
            .goal(task.goal())
            .target_id(task.target_id())
            .target(self.target_registry.get_target(task.target_id()))
            .path(if let Some(subpath) = target.subpath() {
                subpath.to_path_buf()
            } else {
                PathBuf::from(".")
            })
            .workspace_root(workspace.root())
            .build()
            .unwrap();

        let ct = self
            .target_registry
            .associate_concrete_target(task.target_id(), ct);

        let archive = self.archive_manager.download(&target.url()).await?;

        if !fs::try_exists(workspace.root().join(ct.path()))
            .await
            .unwrap_or_default()
        {
            self.archive_manager
                .extract(&archive, workspace.root())
                .await?;
        };

        let sig = self
            .code_manager
            .ready_dependency(task, target, &ct, &archive)
            .await?;

        Ok(sig)
    }
}
