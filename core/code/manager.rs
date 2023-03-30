use super::{CodeDatabase, CodeDatabaseError, SourceHasher, SourceHasherError};
use crate::archive::Archive;
use crate::model::{ConcreteTarget, FsTarget, RemoteTarget, Signature, SourceKind, Task};
use crate::resolver::TargetRegistry;
use crate::store::Store;
use crate::sync::Arc;
use crate::testing::TestMatcherRegistry;
use crate::tricorder::{
    SignatureGenerationFlow, Tricorder, TricorderContext, TricorderError, TricorderManager,
    TricorderManagerError,
};
use crate::worker::{TaskRegistry, TaskResults};
use crate::Config;
use fxhash::FxHashMap;
use std::path::Path;
use thiserror::Error;
use tracing::{debug, info};

pub struct CodeManager<S: Store, T: Tricorder> {
    config: Config,
    db: Arc<CodeDatabase>,
    task_registry: Arc<TaskRegistry>,
    task_results: Arc<TaskResults>,
    test_matcher_registry: Arc<TestMatcherRegistry>,
    tricorder_manager: Arc<TricorderManager<S, T>>,
}

impl<S, T> CodeManager<S, T>
where
    S: Store,
    T: Tricorder + Clone + 'static,
{
    pub fn new(
        config: Config,
        store: Arc<S>,
        test_matcher_registry: Arc<TestMatcherRegistry>,
        target_registry: Arc<TargetRegistry>,
        task_registry: Arc<TaskRegistry>,
        task_results: Arc<TaskResults>,
    ) -> Result<Self, CodeManagerError> {
        let tricorder_context =
            TricorderContext::new(target_registry.clone(), task_registry.clone());

        let tricorder_manager: Arc<TricorderManager<S, T>> = Arc::new(TricorderManager::new(
            config.clone(),
            store,
            tricorder_context,
        ));

        let db = Arc::new(CodeDatabase::new(
            config.clone(),
            test_matcher_registry.clone(),
            target_registry,
            task_registry.clone(),
        )?);

        Ok(Self {
            config,
            db,
            task_registry,
            task_results,
            test_matcher_registry,
            tricorder_manager,
        })
    }

    pub fn tricorder_manager(&self) -> Arc<TricorderManager<S, T>> {
        self.tricorder_manager.clone()
    }

    pub async fn get_signatures(
        &self,
        task: Task,
        target: &FsTarget,
        ct: &ConcreteTarget,
    ) -> Result<SignatureGenerationFlow, CodeManagerError> {
        let whole_source_hash = SourceHasher::hash(target.path()).await?;

        if let Some(signatures) = self
            .db
            .get_signatures(task.goal(), ct, &whole_source_hash)?
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

        let test_matcher = task
            .goal()
            .test_matcher_id()
            .map(|id| self.test_matcher_registry.get(id));

        let deps = self.task_results.get_task_deps(task);

        let subtrees = if let Some(test_matcher) = &test_matcher {
            let subtrees = match tricorder.get_ast(ct, &deps, test_matcher).await? {
                SignatureGenerationFlow::ExtractedAst { subtrees } => subtrees,
                flow @ SignatureGenerationFlow::MissingRequirements { .. } => return Ok(flow),
                _ => unreachable!(),
            };

            let mut signatures = vec![];
            for subtree in &subtrees {
                if let Some(saved_sigs) =
                    self.db
                        .get_signatures(task.goal(), ct, subtree.ast_hash())?
                {
                    signatures.push(saved_sigs);
                }
            }

            if !subtrees.is_empty() && signatures.len() == subtrees.len() {
                let signatures = signatures.concat();
                return Ok(SignatureGenerationFlow::GeneratedSignatures { signatures });
            }

            subtrees
        } else {
            vec![]
        };

        let sig = tricorder
            .generate_signature(ct, &deps, test_matcher)
            .await?;

        if let SignatureGenerationFlow::GeneratedSignatures { signatures } = &sig {
            debug!("Saving signatures for the whole source hash {whole_source_hash}");
            let sig_map: FxHashMap<String, usize> = signatures
                .iter()
                .enumerate()
                .map(|(idx, sig)| (sig.name().to_string(), idx))
                .collect();

            for subtree in subtrees {
                let idx = sig_map.get(subtree.signature_name()).unwrap();
                let sig = signatures[*idx].clone();
                self.db
                    .save_source_chunk(subtree.file(), &sig, subtree.source_chunk())?;
                self.db
                    .save_signatures(task.goal(), ct, subtree.ast_hash(), &[sig])?;
            }
        }

        Ok(sig)
    }

    pub async fn ready_dependency(
        &self,
        task: Task,
        target: &RemoteTarget,
        ct: &ConcreteTarget,
        archive: &Archive,
    ) -> Result<SignatureGenerationFlow, CodeManagerError> {
        if let Some(signatures) = self.db.get_signatures(task.goal(), ct, archive.hash())? {
            return Ok(SignatureGenerationFlow::GeneratedSignatures { signatures });
        }

        let mut tricorder = if let Some(tricorder) = self
            .tricorder_manager
            .find_and_ready(&target.tricorder_url().unwrap())
            .await?
        {
            tricorder
        } else {
            return Ok(SignatureGenerationFlow::IgnoredTarget(task.target_id()));
        };

        let sig = tricorder.ready_dependency(ct, archive).await?;
        Ok(sig)
    }

    pub async fn get_source_chunk(
        &self,
        task: Task,
        sig: &Signature,
        src: &Path,
    ) -> Result<SourceKind, CodeManagerError> {
        let mut tricorder = if let Some(tricorder) = self
            .tricorder_manager
            .find_and_ready_by_path(sig.target().path())
            .await?
        {
            tricorder
        } else {
            unimplemented!()
        };

        let test_matcher = task
            .goal()
            .test_matcher_id()
            .map(|id| self.test_matcher_registry.get(id))
            .unwrap_or_default();

        let subtrees = match tricorder
            .get_ast(sig.target(), &[], &test_matcher)
            .await
            .unwrap()
        {
            SignatureGenerationFlow::ExtractedAst { subtrees } => subtrees,
            _ => unreachable!(),
        };

        info!("go {} subtrees", subtrees.len());
        for subtree in subtrees {
            info!(
                "Subtree {} == {} -> {}",
                subtree.signature_name(),
                sig.name(),
                subtree.signature_name() == sig.name()
            );
            if subtree.signature_name() == sig.name() {
                info!("Using source chunk: {}", subtree.ast_hash());
                self.db
                    .save_source_chunk(subtree.file(), sig, subtree.source_chunk())?;
                return Ok(SourceKind::Chunk(
                    src.to_path_buf(),
                    subtree.source_chunk().to_string(),
                ));
            }
        }
        Ok(SourceKind::File(src.to_path_buf()))
    }
}

#[derive(Error, Debug)]
pub enum CodeManagerError {
    #[error(transparent)]
    CodeDatabaseError(CodeDatabaseError),

    #[error(transparent)]
    TricorderManagerError(TricorderManagerError),

    #[error(transparent)]
    TricorderError(TricorderError),

    #[error(transparent)]
    SourceHasherError(SourceHasherError),
}

impl From<CodeDatabaseError> for CodeManagerError {
    fn from(value: CodeDatabaseError) -> Self {
        CodeManagerError::CodeDatabaseError(value)
    }
}

impl From<TricorderManagerError> for CodeManagerError {
    fn from(value: TricorderManagerError) -> Self {
        CodeManagerError::TricorderManagerError(value)
    }
}

impl From<TricorderError> for CodeManagerError {
    fn from(value: TricorderError) -> Self {
        CodeManagerError::TricorderError(value)
    }
}

impl From<SourceHasherError> for CodeManagerError {
    fn from(value: SourceHasherError) -> Self {
        CodeManagerError::SourceHasherError(value)
    }
}
