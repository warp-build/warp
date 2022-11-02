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
        let local_label = label.get_local().unwrap();
        dbg!(&local_label);

        self.event_channel.send(Event::GeneratingSignature {
            label: label.to_owned(),
        });

        // 1. Register this label as a source file, and store its AST -- at this stage we will be
        //    using tree-sitter to provide us with an very fast and accurate enough representation
        //    of this program.
        //
        let source = self
            .source_manager
            .register_source(label_id, label)
            .await
            .unwrap();

        // 2. Figure out what inside that AST we care about -- we need to know exactly which
        //    signatures we are interested in, and since a source file can have more than one, we
        //    generate a SourceSymbol by inspecting the label and the current goal.
        //
        let symbol = SourceSymbol::from_label_and_goal(label_id, label, self.build_opts.goal);

        // 3. Use the symbol to split the source to the subtree we want to generate a signature
        //    for.
        //
        let source_chunk = self
            .source_manager
            .get_source_chunk_by_symbol(source, &symbol)
            .await
            .unwrap() // chunker error
            .unwrap(); // nothing found

        // 4. Generate a signature for this symbol and this source chunk. This signature will
        //    include all the information we need to build/test/run this chunk.
        //
        let signature = self
            .signature_store
            .generate_signature(label_id, label, &source_chunk, symbol)
            .await
            .unwrap();

        Ok(signature.into())
    }
}
