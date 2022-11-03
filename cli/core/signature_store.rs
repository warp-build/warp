use super::Event;
use super::*;
use dashmap::DashMap;
use std::{path::PathBuf, sync::Arc};
use thiserror::*;
use tokio::fs;
use tokio::io::AsyncReadExt;
use tracing::*;

#[derive(Debug)]
pub struct SignatureStore {
    artifact_store: Arc<ArtifactStore>,
    build_results: Arc<BuildResults>,
    event_channel: Arc<EventChannel>,
    generators: DashMap<String, LabelId>,
    global_signatures_path: PathBuf,
    label_registry: Arc<LabelRegistry>,
    signatures: DashMap<PathBuf, Arc<Vec<Signature>>>,
}

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

    #[error("Could not write signature file at {file:?} due to {err:?}")]
    SignatureWriteError { file: PathBuf, err: std::io::Error },

    #[error("Could not read signature file at {file:?} due to {err:?}")]
    SignatureReadError { file: PathBuf, err: std::io::Error },
}

impl SignatureStore {
    pub fn new(
        workspace: &Workspace,
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
            global_signatures_path: workspace.paths.global_signatures_path.clone(),
        }
    }

    #[tracing::instrument(name = "SignatureStore::generate_signature", skip(self))]
    pub async fn generate_signature(
        &self,
        label_id: LabelId,
        label: &Label,
        source_chunk: &SourceFile,
    ) -> Result<Arc<Vec<Signature>>, SignatureStoreError> {
        let local_label = label.get_local().unwrap();

        let sig_path = self.signature_path(local_label, source_chunk);

        if let Some(signatures) = self.signatures.get(&sig_path) {
            return Ok(signatures.clone());
        }

        let gen_sig = if let Ok(mut file) = fs::File::open(&sig_path).await {
            let mut bytes = vec![];
            file.read_to_end(&mut bytes).await.map_err(|err| {
                SignatureStoreError::SignatureReadError {
                    file: sig_path.clone(),
                    err,
                }
            })?;

            let gen_sig: GeneratedSignature =
                serde_json::from_slice(&bytes).map_err(|err| SignatureStoreError::ParseError {
                    err,
                    json: String::from_utf8(bytes).unwrap(),
                })?;

            gen_sig
        } else {
            let generator = self
                .generators
                .get(&*local_label.extension())
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

            let tmp_dir = tempfile::TempDir::new().unwrap();
            let tmp_src = tmp_dir.path().join(local_label.file());
            fs::create_dir_all(tmp_src.parent().unwrap()).await.unwrap();
            fs::write(&tmp_src, &source_chunk.source).await.unwrap();

            // 2. run generator
            let cmd = CommandRunner::builder()
                .cwd(tmp_dir.path().into())
                .manifest(generator.target_manifest.clone())
                .target(generator.executable_target.clone())
                .args(vec![
                    "generate-signature".to_string(),
                    source_chunk.symbol.to_string(),
                    local_label.file().to_string_lossy().to_string(),
                ])
                .sandboxed(false)
                .stream_outputs(false)
                .build()
                .map_err(SignatureStoreError::CommandRunnerError)?;

            let result = cmd
                .run()
                .await
                .map_err(SignatureStoreError::CommandRunnerError)?;

            let gen_sig: GeneratedSignature = serde_json::from_slice(result.stdout.as_bytes())
                .map_err(|err| SignatureStoreError::ParseError {
                    err,
                    json: result.stdout.clone(),
                })?;

            self.save(local_label, source_chunk, result.stdout.as_bytes())
                .await?;

            gen_sig
        };

        let sigs: Vec<Signature> = gen_sig
            .signatures
            .into_iter()
            .map(|mut sig| {
                sig.name.set_workspace(local_label.workspace());
                for dep in sig.deps.iter_mut().chain(sig.runtime_deps.iter_mut()) {
                    dep.set_workspace(local_label.workspace());
                }
                sig
            })
            .collect();

        if sigs.is_empty() {
            Err(SignatureStoreError::NoSignaturesFound {
                label: label.clone(),
                symbol: source_chunk.symbol.clone(),
            })
        } else {
            let sigs = Arc::new(sigs);
            self.signatures.insert(sig_path, sigs.clone());
            Ok(sigs)
        }
    }

    async fn save(
        &self,
        label: &LocalLabel,
        source_chunk: &SourceFile,
        sig: &[u8],
    ) -> Result<(), SignatureStoreError> {
        let sig_path = self.signature_path(label, source_chunk);

        fs::write(&sig_path, sig)
            .await
            .map_err(|err| SignatureStoreError::SignatureWriteError {
                file: sig_path.clone(),
                err,
            })
    }

    fn signature_path(&self, label: &LocalLabel, source_chunk: &SourceFile) -> PathBuf {
        self.global_signatures_path
            .join(format!(
                "{:x}-{}-{}",
                label.hash(),
                source_chunk.hash,
                label.name().unwrap()
            ))
            .with_extension("wsig")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
}
