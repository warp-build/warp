use super::Event;
use super::*;
use dashmap::DashMap;
use std::{path::PathBuf, sync::Arc};
use thiserror::*;
use tracing::*;

#[derive(Error, Debug)]
pub enum SignatureStoreError {
    #[error("We don't yet have a signature generator built!")]
    MissingGenerator { generator: LabelId },

    #[error(transparent)]
    CommandRunnerError(CommandRunnerError),

    #[error("Could not parse Generated Signature: \n\n{json}\n\ndue to {err:#?}")]
    ParseError {
        err: serde_json::Error,
        json: String,
    },

    #[error("Could not find any generated signatures for {} using {symbol:?}", label.to_string())]
    NoSignaturesFound { label: Label, symbol: SourceSymbol },

    #[error("No generator was registered for files of extension {}, so we can't generate a signature for {}", label.extension().unwrap(), label.to_string())]
    UnknownSignature { label: Label },
}

#[derive(Debug)]
pub struct SignatureStore {
    signatures: DashMap<(LabelId, SourceId, SourceSymbol), Signature>,
    build_results: Arc<BuildResults>,
    label_registry: Arc<LabelRegistry>,
    event_channel: Arc<EventChannel>,
    artifact_store: Arc<ArtifactStore>,
    generators: DashMap<String, LabelId>,
}

impl SignatureStore {
    pub fn new(
        build_results: Arc<BuildResults>,
        event_channel: Arc<EventChannel>,
        artifact_store: Arc<ArtifactStore>,
        label_registry: Arc<LabelRegistry>,
    ) -> Self {
        let generators = DashMap::new();
        for (ext, generator) in &[
            ("erl", "https://tools.warp.build/erlang/lifter"),
            ("hrl", "https://tools.warp.build/erlang/lifter"),
        ] {
            let label: Label = generator.parse::<url::Url>().unwrap().into();
            let label = label_registry.register_label(label);
            generators.insert(ext.to_string(), label);
        }

        Self {
            signatures: DashMap::default(),
            build_results,
            event_channel,
            artifact_store,
            label_registry,
            generators,
        }
    }

    #[tracing::instrument(name = "SignatureStore::generate_signature", skip(self))]
    pub async fn generate_signature(
        &self,
        label_id: LabelId,
        label: &Label,
        source_chunk: &SourceId,
        symbol: SourceSymbol,
    ) -> Result<Signature, SignatureStoreError> {
        let generator = self
            .generators
            .get(&label.extension().unwrap())
            .map(|r| (*r))
            .ok_or_else(|| SignatureStoreError::UnknownSignature {
                label: label.clone(),
            })?;

        // 1. build signature generator
        let generator = self
            .build_results
            .get_build_result(generator)
            .ok_or(SignatureStoreError::MissingGenerator { generator })?;

        self.event_channel.send(Event::GeneratingSignature {
            label: label.to_owned(),
        });

        // 2. run generator
        let cmd = CommandRunner::builder()
            .cwd(PathBuf::from("."))
            .manifest(generator.target_manifest.clone())
            .target(generator.executable_target.clone())
            .args(vec![
                "generate-signatures".to_string(),
                label.path().to_str().unwrap().to_string(),
            ])
            .sandboxed(false)
            .stream_outputs(false)
            .build()
            .map_err(SignatureStoreError::CommandRunnerError)?;

        let json = cmd
            .run()
            .await
            .map_err(SignatureStoreError::CommandRunnerError)?;

        let gen_sig: GeneratedSignature = serde_json::from_slice(json.as_bytes())
            .map_err(|err| SignatureStoreError::ParseError { err, json })?;

        gen_sig
            .signatures
            .get(0)
            .cloned()
            .ok_or_else(|| SignatureStoreError::NoSignaturesFound {
                label: label.clone(),
                symbol,
            })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
}
