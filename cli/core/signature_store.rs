use super::Event;
use super::*;
use dashmap::DashMap;
use std::collections::BTreeMap;
use std::{path::PathBuf, sync::Arc};
use thiserror::*;
use tokio::fs;
use tokio::io::AsyncReadExt;
use tracing::*;

#[derive(Debug)]
pub struct SignatureStore {
    analyzer_service_manager: Arc<AnalyzerServiceManager>,

    artifact_store: Arc<ArtifactStore>,

    build_results: Arc<BuildResults>,

    event_channel: Arc<EventChannel>,

    generators: DashMap<String, LabelId>,

    global_signatures_path: PathBuf,

    label_registry: Arc<LabelRegistry>,

    signatures: DashMap<PathBuf, Arc<Vec<Signature>>>,
    workspace: Workspace,
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

    #[error("Failed to generate signature for {}. Generator exited with status {status}.\n\nStdout: {stdout}\n\nStderr: {stderr}", label.to_string())]
    GeneratorError {
        stdout: String,
        stderr: String,
        status: i32,
        label: Label,
    },

    #[error(transparent)]
    AnalyzerServiceManagerError(AnalyzerServiceManagerError),

    #[error(transparent)]
    AnalyzerServiceError(tonic::Status),
}

impl SignatureStore {
    pub fn new(
        workspace: &Workspace,
        build_results: Arc<BuildResults>,
        event_channel: Arc<EventChannel>,
        artifact_store: Arc<ArtifactStore>,
        label_registry: Arc<LabelRegistry>,
        analyzer_service_manager: Arc<AnalyzerServiceManager>,
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
            analyzer_service_manager,
            artifact_store,
            build_results,
            event_channel,
            generators,
            global_signatures_path: workspace.paths.global_signatures_path.clone(),
            label_registry,
            signatures: DashMap::default(),
            workspace: workspace.to_owned(),
        }
    }

    pub async fn generate_signature(
        &self,
        _label_id: LabelId,
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
        } else if let Some(analyzer_id) = self
            .analyzer_service_manager
            .find_analyzer_for_local_label(local_label)
        {
            let code_db = CodeDb::new(&self.workspace).await.unwrap();

            let mut analyzer_svc = self
                .analyzer_service_manager
                .start(analyzer_id)
                .await
                .map_err(SignatureStoreError::AnalyzerServiceManagerError)?;

            self.event_channel.send(Event::GeneratingSignature {
                label: label.to_owned(),
            });

            let request = proto::build::warp::codedb::GenerateSignatureRequest {
                file: local_label.file().to_string_lossy().to_string(),
            };
            let response = analyzer_svc
                .generate_signature(request)
                .await
                .map_err(SignatureStoreError::AnalyzerServiceError)?
                .into_inner();

            let mut signatures = vec![];
            for sig in response.signatures {
                let mut deps = vec![];
                for dep in sig.deps {
                    let req = dep.requirement.unwrap();
                    match req {
                        proto::build::warp::requirement::Requirement::File(file_req) => {
                            /*
                            let label = code_db.find_label_for_file(&file_req.path).unwrap();
                            deps.push(label)
                            */
                        }
                        proto::build::warp::requirement::Requirement::Symbol(sym_req) => {
                            let label = code_db
                                .find_label_for_symbol(&sym_req.raw, &sym_req.kind)
                                .await
                                .unwrap();
                            deps.push(label)
                        }
                    }
                }

                let mut runtime_deps = vec![];
                for dep in sig.runtime_deps {
                    let req = dep.requirement.unwrap();
                    match req {
                        proto::build::warp::requirement::Requirement::File(file_req) => {
                            /*
                            let label = code_db.find_label_for_file(&file_req.path).unwrap();
                            deps.push(label)
                            */
                        }
                        proto::build::warp::requirement::Requirement::Symbol(sym_req) => {
                            let label = code_db
                                .find_label_for_symbol(&sym_req.raw, &sym_req.kind)
                                .await
                                .unwrap();
                            runtime_deps.push(label)
                        }
                    }
                }

                let signature = Signature {
                    name: sig.name.parse().unwrap(),
                    rule: sig.rule,
                    deps,
                    runtime_deps,
                    config: sig.config.map(|c| c.into()).unwrap_or_default(),
                };

                signatures.push(signature)
            }

            let gen_sig = GeneratedSignature {
                version: 0,
                signatures,
            };

            let json_gen_sig = serde_json::to_string_pretty(&gen_sig).unwrap();

            self._save(local_label, source_chunk, json_gen_sig.as_bytes())
                .await?;

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
            let tmp_dir = tmp_dir.into_path();
            let tmp_src = tmp_dir.as_path().join(local_label.file());
            fs::create_dir_all(tmp_src.parent().unwrap()).await.unwrap();
            fs::write(&tmp_src, &source_chunk.source).await.unwrap();

            // 2. run generator
            let cmd = CommandRunner::builder()
                .cwd(tmp_dir.as_path().into())
                .manifest(generator.target_manifest.clone())
                .target(generator.executable_target.clone())
                .args(vec![
                    "generate-signature".to_string(),
                    source_chunk.symbol.to_string(),
                    local_label.file().to_string_lossy().to_string(),
                ])
                .sandboxed(true)
                .stream_outputs(false)
                .build()
                .map_err(SignatureStoreError::CommandRunnerError)?;

            let result = cmd
                .run()
                .await
                .map_err(SignatureStoreError::CommandRunnerError)?;

            if result.status != 0 {
                return Err(SignatureStoreError::GeneratorError {
                    stdout: result.stdout,
                    stderr: result.stderr,
                    status: result.status,
                    label: local_label.to_owned().into(),
                });
            }

            let gen_sig: GeneratedSignature = serde_json::from_slice(result.stdout.as_bytes())
                .map_err(|err| SignatureStoreError::ParseError {
                    err,
                    json: result.stdout.clone(),
                })?;

            self._save(local_label, source_chunk, result.stdout.as_bytes())
                .await?;

            gen_sig
        };

        // NOTE(@ostera): we support a small file called <filename>.warp that can contain hints for
        // the build system. These are things that we can't tell by static analysis of the file,
        // or that would otherwise be very tricky to reliably get.
        //
        let hints_path = PathBuf::from(format!("{}.warp", local_label.file().to_string_lossy()));
        let hints: BTreeMap<String, RuleConfig> = if let Ok(mut file) =
            fs::File::open(&hints_path).await
        {
            let mut bytes = vec![];
            file.read_to_end(&mut bytes).await.map_err(|err| {
                SignatureStoreError::SignatureReadError {
                    file: sig_path.clone(),
                    err,
                }
            })?;

            let hints: BTreeMap<String, RuleConfig> =
                serde_json::from_slice(&bytes).map_err(|err| SignatureStoreError::ParseError {
                    err,
                    json: String::from_utf8(bytes).unwrap(),
                })?;

            hints
        } else {
            BTreeMap::new()
        };

        let mut sigs: Vec<Signature> = vec![];

        for mut sig in gen_sig.signatures.into_iter() {
            sig.name.set_workspace(local_label.workspace());

            // NOTE(@ostera): if there are any hints, we will put them at the back of the
            // dependency and runtime dependency lists.
            //
            if let Some(cfg) = hints.get(&*sig.name.name()) {
                let package_path = local_label.file().parent().unwrap();
                for mut dep in cfg.get_label_list("deps").unwrap_or_default() {
                    if dep.path().starts_with("./") {
                        dep.set_path(
                            package_path.join(dep.path().to_string_lossy().replace("./", "")),
                        );
                        dep = dep.to_abstract().unwrap();
                    }
                    sig.deps.push(dep);
                }

                for mut dep in cfg.get_label_list("runtime_deps").unwrap_or_default() {
                    if dep.path().starts_with("./") {
                        dep.set_path(
                            package_path.join(dep.path().to_string_lossy().replace("./", "")),
                        );
                        dep = dep.to_abstract().unwrap();
                    }
                    sig.runtime_deps.push(dep);
                }
            };

            for dep in sig.deps.iter_mut().chain(sig.runtime_deps.iter_mut()) {
                dep.set_workspace(local_label.workspace());
            }

            sigs.push(sig)
        }

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

    pub async fn save<S>(
        &self,
        label: &LocalLabel,
        source_chunk: &SourceFile,
        signature: S,
    ) -> Result<(), SignatureStoreError>
    where
        S: Into<GeneratedSignature>,
    {
        let json = serde_json::to_string_pretty(&signature.into()).unwrap();
        self._save(label, source_chunk, json.as_bytes()).await
    }

    async fn _save(
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
                source_chunk.ast_hash,
                label.name().unwrap().replace('/', "_")
            ))
            .with_extension("wsig")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
}
