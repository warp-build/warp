use std::time::Duration;

use super::{ResolverError, TargetRegistry};
use crate::archive::ArchiveManager;
use crate::code::CodeDatabase;
use crate::model::{ConcreteTarget, RemoteTarget, TargetId};
use crate::store::DefaultStore;
use crate::sync::Arc;
use crate::tricorder::{SignatureGenerationFlow, Tricorder, TricorderManager};
use crate::workspace::WorkspaceManager;
use crate::{Config, Goal};
use futures::TryFutureExt;
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
pub struct NetResolver<T: Tricorder> {
    config: Config,
    archive_manager: Arc<ArchiveManager>,
    workspace_manager: Arc<WorkspaceManager>,
    tricorder_manager: Arc<TricorderManager<T, DefaultStore>>,
    target_registry: Arc<TargetRegistry>,
    code_db: Arc<CodeDatabase>,
}

impl<T: Tricorder + Clone + 'static> NetResolver<T> {
    pub fn new(
        config: Config,
        archive_manager: Arc<ArchiveManager>,
        workspace_manager: Arc<WorkspaceManager>,
        tricorder_manager: Arc<TricorderManager<T, DefaultStore>>,
        target_registry: Arc<TargetRegistry>,
        code_db: Arc<CodeDatabase>,
    ) -> Self {
        Self {
            config,
            archive_manager,
            workspace_manager,
            tricorder_manager,
            target_registry,
            code_db,
        }
    }

    #[instrument(name = "NetResolver::resolve", skip(self))]
    pub async fn resolve(
        &self,
        goal: Goal,
        target_id: TargetId,
        target: &RemoteTarget,
    ) -> Result<SignatureGenerationFlow, ResolverError> {
        let workspace_id = self
            .workspace_manager
            .register_remote_workspace(&target.url());
        let workspace = self.workspace_manager.get_workspace(workspace_id);

        let ct = ConcreteTarget::builder()
            .goal(goal)
            .target_id(target_id)
            .target(self.target_registry.get_target(target_id))
            .path(target.subpath().unwrap())
            .workspace_root(workspace.root())
            .build()
            .unwrap();

        let ct = self
            .target_registry
            .associate_concrete_target(target_id, ct);

        let archive = if let Some(mut archive) = self.archive_manager.find(&target.url()).await? {
            archive.set_final_path(workspace.root().to_path_buf());
            archive
        } else {
            let archive = self.archive_manager.download(&target.url()).await?;
            self.archive_manager
                .extract(&archive, workspace.root())
                .await?
        };

        if let Some(mut signature) = self.code_db.get_signature(&ct, archive.hash())? {
            let mut ct = signature.target().clone();
            ct.set_target(self.target_registry.get_target(target_id), target_id);
            let ct = self
                .target_registry
                .associate_concrete_target(target_id, ct);
            signature.set_target((*ct).clone());
            return Ok(SignatureGenerationFlow::GeneratedSignatures {
                signatures: vec![signature],
            });
        }

        let mut tricorder = if let Some(tricorder) = self
            .tricorder_manager
            .find_and_ready(&target.tricorder_url().unwrap())
            .await?
        {
            tricorder
        } else {
            return Ok(SignatureGenerationFlow::IgnoredTarget(target_id));
        };

        let sig = tricorder.ready_dependency(&ct, &archive).await?;

        Ok(sig)
    }
}
