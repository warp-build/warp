use super::{ResolverError, TargetRegistry};
use crate::code::{CodeDatabase, SourceHasher};
use crate::model::{ConcreteTarget, FsTarget, Task};
use crate::store::DefaultStore;
use crate::sync::*;
use crate::testing::TestMatcherRegistry;
use crate::tricorder::{SignatureGenerationFlow, Tricorder, TricorderManager};
use crate::worker::TaskResults;
use crate::workspace::WorkspaceManager;
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
    code_db: Arc<CodeDatabase>,
    target_registry: Arc<TargetRegistry>,
    task_results: Arc<TaskResults>,
    test_matcher_registry: Arc<TestMatcherRegistry>,
    tricorder_manager: Arc<TricorderManager<T, DefaultStore>>,
    workspace_manager: Arc<WorkspaceManager>,
}

impl<T: Tricorder + Clone + 'static> FsResolver<T> {
    pub fn new(
        workspace_manager: Arc<WorkspaceManager>,
        tricorder_manager: Arc<TricorderManager<T, DefaultStore>>,
        target_registry: Arc<TargetRegistry>,
        test_matcher_registry: Arc<TestMatcherRegistry>,
        task_results: Arc<TaskResults>,
        code_db: Arc<CodeDatabase>,
    ) -> Self {
        Self {
            code_db,
            target_registry,
            task_results,
            test_matcher_registry,
            tricorder_manager,
            workspace_manager,
        }
    }

    #[instrument(name = "FsResolver::resolve", skip(self))]
    pub async fn resolve(
        &self,
        task: Task,
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

        // goal -- test { matcher_id }
        let test_matcher = task
            .goal()
            .test_matcher_id()
            .map(|id| self.test_matcher_registry.get_spec(id));

        let source_hash = SourceHasher::hash(&final_path).await?;

        let ct = ConcreteTarget::builder()
            .goal(task.goal())
            .target_id(task.target_id())
            .target(self.target_registry.get_target(task.target_id()))
            .path(final_path)
            .workspace_root(workspace.root())
            .build()
            .unwrap();

        let ct = self
            .target_registry
            .associate_concrete_target(task.target_id(), ct);

        if let Some(mut signature) = self.code_db.get_signature(&ct, &source_hash)? {
            let mut ct = signature.target().clone();
            ct.set_target(
                self.target_registry.get_target(task.target_id()),
                task.target_id(),
            );
            let ct = self
                .target_registry
                .associate_concrete_target(task.target_id(), ct);
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
            return Ok(SignatureGenerationFlow::IgnoredTarget(task.target_id()));
        };

        // TODO(@ostera): at this stage, we want to use the concrete target and the tricorder to
        // call the CodeManager and ask it to tree-split, so we can avoid regenerating signatures
        // if parts of the file we don't care about haven't changed.
        // get_ast

        // 2. generate signature for this concrete target
        let deps = self.task_results.get_task_deps(task);
        let sig = tricorder
            .generate_signature(&ct, &deps, test_matcher)
            .await?;

        // NB(@ostera): we'll just save the first signature here.
        if let SignatureGenerationFlow::GeneratedSignatures { signatures } = &sig {
            let sig = signatures.iter().next().unwrap();
            self.code_db.save_signature(&ct, &source_hash, sig)?;
        }

        Ok(sig)
    }
}
