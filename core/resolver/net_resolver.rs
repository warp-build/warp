use super::{ResolverError, TargetRegistry};
use crate::archive::ArchiveManager;
use crate::code::CodeManager;
use crate::model::{ConcreteTarget, RemoteTarget, Task};
use crate::store::Store;
use crate::sync::Arc;
use crate::tricorder::{SignatureGenerationFlow, Tricorder};
use crate::workspace::WorkspaceManager;
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
            .path(target.subpath().unwrap())
            .workspace_root(workspace.root())
            .build()
            .unwrap();

        let ct = self
            .target_registry
            .associate_concrete_target(task.target_id(), ct);

        let archive = if let Some(mut archive) = self.archive_manager.find(&target.url()).await? {
            archive.set_final_path(workspace.root().to_path_buf());
            archive
        } else {
            let archive = self.archive_manager.download(&target.url()).await?;
            self.archive_manager
                .extract(&archive, workspace.root())
                .await?
        };

        let sig = self
            .code_manager
            .ready_dependency(task, target, &ct, &archive)
            .await?;

        Ok(sig)
    }
}
