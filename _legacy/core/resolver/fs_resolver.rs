use super::{ResolverError, TargetRegistry};
use crate::code::CodeManager;
use crate::model::{ConcreteTarget, FsTarget, Task};
use crate::store::Store;
use crate::sync::*;
use crate::testing::TestMatcherRegistry;
use crate::tricorder::{SignatureGenerationFlow, Tricorder};
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
pub struct FsResolver<S: Store, T: Tricorder> {
    target_registry: Arc<TargetRegistry>,
    task_results: Arc<TaskResults>,
    test_matcher_registry: Arc<TestMatcherRegistry>,
    code_manager: Arc<CodeManager<S, T>>,
    workspace_manager: Arc<WorkspaceManager>,
}

impl<S, T> FsResolver<S, T>
where
    S: Store,
    T: Tricorder + Clone + 'static,
{
    pub fn new(
        workspace_manager: Arc<WorkspaceManager>,
        target_registry: Arc<TargetRegistry>,
        test_matcher_registry: Arc<TestMatcherRegistry>,
        task_results: Arc<TaskResults>,
        code_manager: Arc<CodeManager<S, T>>,
    ) -> Self {
        Self {
            target_registry,
            task_results,
            test_matcher_registry,
            code_manager,
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
                target: target.clone(),
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

        let sig = self.code_manager.get_signatures(task, target, &ct).await?;

        Ok(sig)
    }
}
