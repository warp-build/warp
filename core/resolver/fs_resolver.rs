use super::{ResolverError, TargetRegistry};
use crate::code::{CodeDatabase, SourceHasher};
use crate::model::{ConcreteTarget, FsTarget, Task};
use crate::store::DefaultStore;
use crate::sync::*;
use crate::testing::TestMatcherRegistry;
use crate::tricorder::{SignatureGenerationFlow, Subtree, Tricorder, TricorderManager};
use crate::worker::TaskResults;
use crate::workspace::WorkspaceManager;
use fxhash::FxHashMap;
use tracing::{debug, instrument};

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

        let deps = self.task_results.get_task_deps(task);

        let test_matcher = task
            .goal()
            .test_matcher_id()
            .map(|id| self.test_matcher_registry.get(id));

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

        let whole_source_hash = SourceHasher::hash(&target.path()).await?;

        if let Some(signatures) =
            self.code_db
                .get_signatures(task.goal(), &ct, &whole_source_hash)?
        {
            return Ok(SignatureGenerationFlow::GeneratedSignatures { signatures });
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

        let save_strategy = if let Some(test_matcher) = &test_matcher {
            let subtrees = match tricorder.get_ast(&ct, &deps, test_matcher).await? {
                SignatureGenerationFlow::ExtractedAst { subtrees } => subtrees,
                flow @ SignatureGenerationFlow::MissingRequirements { .. } => return Ok(flow),
                _ => unreachable!(),
            };

            let mut signatures = vec![];
            for subtree in &subtrees {
                if let Some(saved_sigs) =
                    self.code_db
                        .get_signatures(task.goal(), &ct, subtree.ast_hash())?
                {
                    signatures.push(saved_sigs);
                }
            }

            if signatures.len() == subtrees.len() {
                let signatures = signatures.concat();
                return Ok(SignatureGenerationFlow::GeneratedSignatures { signatures });
            }

            SignatureSaveStrategy::Subtrees(subtrees)
        } else {
            SignatureSaveStrategy::WholeSource(whole_source_hash)
        };

        let sig = tricorder
            .generate_signature(&ct, &deps, test_matcher)
            .await?;

        if let SignatureGenerationFlow::GeneratedSignatures { signatures } = &sig {
            match save_strategy {
                SignatureSaveStrategy::WholeSource(whole_source_hash) => {
                    debug!("Saving signatures for the whole source hash {whole_source_hash}");
                    self.code_db.save_signatures(
                        task.goal(),
                        &ct,
                        &whole_source_hash,
                        signatures,
                    )?
                }

                SignatureSaveStrategy::Subtrees(subtree_pairs) => {
                    let sig_map: FxHashMap<String, usize> = signatures
                        .iter()
                        .enumerate()
                        .map(|(idx, sig)| (sig.name().to_string(), idx))
                        .collect();

                    for subtree in subtree_pairs {
                        let idx = sig_map.get(subtree.signature_name()).unwrap();
                        let sig = signatures[*idx].clone();
                        self.code_db.save_signatures(
                            task.goal(),
                            &ct,
                            subtree.ast_hash(),
                            &[sig],
                        )?;
                    }
                }
            }
        }

        Ok(sig)
    }
}

enum SignatureSaveStrategy {
    Subtrees(Vec<Subtree>),
    WholeSource(String),
}
