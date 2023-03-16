use super::{ResolverError, TargetRegistry};
use crate::code::{CodeDatabase, SourceHasher};
use crate::model::{ConcreteTarget, FsTarget, Goal, TargetId};
use crate::store::DefaultStore;
use crate::tricorder::{SignatureGenerationFlow, Tricorder, TricorderManager};
use crate::worker::TaskResults;
use crate::workspace::WorkspaceManager;
use crate::{sync::*, Config};
use tracing::instrument;

#[cfg_attr(doc, aquamarine::aquamarine)]
/// File System Resolver for Local Targets.
///
/// The `FsResolver` knows how to resolve a particular `Target` by looking into the file system and
/// determining if this is in fact a file on disk, and sending the sources to a `Tricorder` for
/// analysis.
///
/// It's flow goes:
///
/// ```mermaid
/// graph TD
/// ```
#[derive(Clone)]
pub struct FsResolver<T: Tricorder> {
    config: Config,
    target_registry: Arc<TargetRegistry>,
    workspace_manager: Arc<WorkspaceManager>,
    tricorder_manager: Arc<TricorderManager<T, DefaultStore>>,
    task_results: Arc<TaskResults>,
    code_db: Arc<CodeDatabase>,
}

impl<T: Tricorder + Clone + 'static> FsResolver<T> {
    pub fn new(
        config: Config,
        workspace_manager: Arc<WorkspaceManager>,
        tricorder_manager: Arc<TricorderManager<T, DefaultStore>>,
        target_registry: Arc<TargetRegistry>,
        task_results: Arc<TaskResults>,
        code_db: Arc<CodeDatabase>,
    ) -> Self {
        Self {
            config,
            workspace_manager,
            tricorder_manager,
            target_registry,
            task_results,
            code_db,
        }
    }

    #[instrument(name = "FsResolver::resolve", skip(self))]
    pub async fn resolve(
        &self,
        goal: Goal,
        target_id: TargetId,
        target: &FsTarget,
    ) -> Result<SignatureGenerationFlow, ResolverError> {
        // NB: early return in case we can't find the file, since FsTarget's are supposed to exist
        // within the file system already.
        if tokio::fs::metadata(target.path()).await.is_err() {
            return Err(ResolverError::CouldNotFindFile {
                path: target.path().clone(),
            });
        }

        let workspace = self.workspace_manager.current_workspace();
        let final_path = if target.path().starts_with(workspace.root()) {
            target
                .path()
                .strip_prefix(workspace.root())
                .unwrap()
                .to_path_buf()
        } else {
            target.path().to_path_buf()
        };

        let source_hash = SourceHasher::hash(&final_path).await?;
        let ct = ConcreteTarget::builder()
            .goal(goal)
            .target_id(target_id)
            .target(self.target_registry.get_target(target_id))
            .path(final_path)
            .workspace_root(workspace.root())
            .build()
            .unwrap();

        let ct = self
            .target_registry
            .associate_concrete_target(target_id, ct);

        if let Some(mut signature) = self.code_db.get_signature(&ct, &source_hash)? {
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
            .find_and_ready_by_path(ct.path())
            .await?
        {
            tricorder
        } else {
            return Ok(SignatureGenerationFlow::IgnoredTarget(target_id));
        };

        // TODO(@ostera): at this stage, we want to use the concrete target and the tricorder to
        // call the CodeManager and ask it to tree-split, so we can avoid regenerating signatures
        // if parts of the file we don't care about haven't changed.
        // get_ast

        // 2. generate signature for this concrete target
        let deps = self.task_results.get_task_deps(target_id);
        let sig = tricorder.generate_signature(&ct, &deps).await?;

        // NB(@ostera): we'll just save the first signature here.
        if let SignatureGenerationFlow::GeneratedSignatures { signatures } = &sig {
            let sig = signatures.iter().next().unwrap();
            self.code_db.save_signature(&ct, &source_hash, sig)?;
        }

        Ok(sig)
    }
}
