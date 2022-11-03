use std::sync::Arc;

use super::Event;
use super::*;

/// A Label resolver capable of analyzing the sources that a label points to, to understand how to
/// build it.
///
#[derive(Debug, Clone)]
pub struct SourceResolver {
    build_opts: BuildOpts,
    event_channel: Arc<EventChannel>,
    label_registry: Arc<LabelRegistry>,
    signature_store: Arc<SignatureStore>,
    source_manager: Arc<SourceManager>,
}

impl SourceResolver {
    pub fn new(
        event_channel: Arc<EventChannel>,
        label_registry: Arc<LabelRegistry>,
        source_manager: Arc<SourceManager>,
        signature_store: Arc<SignatureStore>,
        build_opts: BuildOpts,
    ) -> Self {
        Self {
            event_channel,
            label_registry,
            source_manager,
            signature_store,
            build_opts,
        }
    }

    #[tracing::instrument(name = "SourceResolver::resolve", skip(self))]
    pub async fn resolve(
        &self,
        label_id: LabelId,
        label: &Label,
    ) -> Result<Target, LabelResolverError> {
        let _local_label = label.get_local().unwrap();

        // 1. Figure out what inside that AST we care about -- we need to know exactly which
        //    signatures we are interested in, and since a source file can have more than one, we
        //    generate a SourceSymbol by inspecting the label and the current goal.
        //
        let symbol = SourceSymbol::from_label_and_goal(label_id, label, self.build_opts.goal);

        // 2. Use the symbol to split the source to the subtree we want to generate a signature
        //    for. at this stage we will be using tree-sitter to provide us with an very fast and
        //    accurate enough representation of this program.
        //
        let source_chunk = self
            .source_manager
            .get_source_chunk_by_symbol(label_id, label, &symbol)
            .await
            .map_err(LabelResolverError::SourceManagerError)?;

        // 4. Generate a signature for this symbol and this source chunk. This signature will
        //    include all the information we need to build/test/run this chunk.
        //
        let signatures = self
            .signature_store
            .generate_signature(label_id, label, &source_chunk)
            .await
            .map_err(LabelResolverError::SignatureStoreError)?;

        for sig in signatures.iter() {
            if self.build_opts.goal.includes(&sig) && sig.name.name() == label.name() {
                return Ok(sig.to_owned().into());
            }
        }

        Err(LabelResolverError::SourceManagerError(
            SourceManagerError::BadSignature,
        ))
    }
}
