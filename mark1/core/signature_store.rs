use super::Event;
use super::*;
use dashmap::{DashMap, DashSet};
use std::collections::BTreeMap;
use std::{path::PathBuf, sync::Arc};
use thiserror::*;
use tokio::fs;
use tokio::io::AsyncReadExt;
use tracing::*;

type SignatureKey = PathBuf;

#[derive(Debug)]
pub struct SignatureStore {
    analyzer_service_manager: Arc<AnalyzerServiceManager>,

    artifact_store: Arc<ArtifactStore>,

    build_results: Arc<BuildResults>,

    event_channel: Arc<EventChannel>,

    global_signatures_path: PathBuf,

    label_registry: Arc<LabelRegistry>,

    dependency_manager: Arc<DependencyManager>,

    signatures: DashMap<SignatureKey, Arc<Vec<Signature>>>,

    confirmed_signatures: DashSet<LabelId>,

    workspace: Workspace,

    build_opts: BuildOpts,
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

    #[error(transparent)]
    CodeDbError(CodeDbError),

    #[error("Could not find an analyzer suitable for {}", .label.to_string())]
    MissingAnalyzer { label: Label },

    #[error("Could not generate signature for {} due to missing dependencies: {dep_labels:?}", .label.to_string())]
    MissingDeps {
        label: Label,
        dep_ids: Vec<LabelId>,
        dep_labels: Vec<Label>,
    },

    #[error(transparent)]
    ArtifactStoreError(ArtifactStoreError),
}

impl SignatureStore {
    pub fn new(
        workspace: &Workspace,
        build_results: Arc<BuildResults>,
        event_channel: Arc<EventChannel>,
        artifact_store: Arc<ArtifactStore>,
        label_registry: Arc<LabelRegistry>,
        analyzer_service_manager: Arc<AnalyzerServiceManager>,
        dependency_manager: Arc<DependencyManager>,
        build_opts: BuildOpts,
    ) -> Self {
        Self {
            analyzer_service_manager,
            build_results,
            dependency_manager,
            artifact_store,
            event_channel,
            global_signatures_path: workspace.paths.global_signatures_path.clone(),
            label_registry,
            signatures: DashMap::default(),
            confirmed_signatures: DashSet::default(),
            workspace: workspace.to_owned(),
            build_opts,
        }
    }

    pub fn confirm_signature(&self, label_id: LabelId) {
        self.confirmed_signatures.insert(label_id);
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

        if self.build_opts.experimental_regenerate_signatures {
            let _ = fs::remove_file(&sig_path).await;
            self.signatures.remove(&sig_path);
        }

        if self.confirmed_signatures.contains(&label_id) {
            if let Some(signatures) = self.signatures.get(&sig_path) {
                return Ok(signatures.clone());
            }
        }

        let gen_sig = if let Ok(gen_sig) = self.read_signature_from_cache(&sig_path).await {
            gen_sig
        } else {
            self.do_generate_sigature(label, local_label, source_chunk)
                .await?
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

            // NOTE(@ostera): if there are any hints for _all targets_ (designated with the `*`
            // key), put them into the dependency and runtime dependency lists.
            //
            if let Some(cfg) = hints.get("*") {
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
            self.signatures.insert(sig_path.clone(), sigs.clone());
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

    pub async fn find_signature(
        &self,
        label: &LocalLabel,
        source_chunk: &SourceFile,
    ) -> Result<Option<GeneratedSignature>, SignatureStoreError> {
        let sig_path = self.signature_path(label, source_chunk);

        if let Ok(mut file) = fs::File::open(&sig_path).await {
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

            Ok(Some(gen_sig))
        } else {
            Ok(None)
        }
    }

    #[tracing::instrument(name = "SignatureStore::read_signature_from_cache", skip(self))]
    async fn read_signature_from_cache(
        &self,
        sig_path: &PathBuf,
    ) -> Result<GeneratedSignature, SignatureStoreError> {
        let mut file = fs::File::open(&sig_path).await.map_err(|err| {
            SignatureStoreError::SignatureReadError {
                file: sig_path.clone(),
                err,
            }
        })?;

        let mut bytes = vec![];
        file.read_to_end(&mut bytes).await.map_err(|err| {
            SignatureStoreError::SignatureReadError {
                file: sig_path.clone(),
                err,
            }
        })?;

        serde_json::from_slice(&bytes).map_err(|err| SignatureStoreError::ParseError {
            err,
            json: String::from_utf8(bytes).unwrap(),
        })
    }

    #[tracing::instrument(
        name = "SignatureStore::do_generate_signature",
        skip(self, source_chunk)
    )]
    async fn do_generate_sigature(
        &self,
        label: &Label,
        local_label: &LocalLabel,
        source_chunk: &SourceFile,
    ) -> Result<GeneratedSignature, SignatureStoreError> {
        let analyzer_id = self
            .analyzer_service_manager
            .find_analyzer_for_local_label(local_label)
            .ok_or_else(|| SignatureStoreError::MissingAnalyzer {
                label: label.clone(),
            })?;

        let mut analyzer_svc = self
            .analyzer_service_manager
            .start(analyzer_id)
            .await
            .map_err(SignatureStoreError::AnalyzerServiceManagerError)?;

        let dependencies = self.find_dep_paths().await?;

        let request = proto::build::warp::codedb::GenerateSignatureRequest {
            file: local_label.file().to_string_lossy().to_string(),
            symbol: Some(source_chunk.symbol.clone().into()),
            dependencies,
        };
        let response = analyzer_svc
            .generate_signature(request)
            .await
            .map_err(SignatureStoreError::AnalyzerServiceError)?
            .into_inner()
            .response
            .unwrap();

        let code_db = CodeDb::new(&self.workspace).await.unwrap();
        let mut signatures = vec![];
        match response {
            proto::build::warp::codedb::generate_signature_response::Response::Ok(response) => {
                for sig in response.signatures {
                    let deps: DashSet<Label> = DashSet::new();
                    for dep in sig.deps {
                        let req = dep.requirement.unwrap();

                        match req {
                            proto::build::warp::requirement::Requirement::Url(url_req) => {
                                let url: url::Url = url_req.url.parse().unwrap();
                                let label: Label = url.clone().into();
                                debug!("DEP: {} -> {}", &url, label.to_string());
                                deps.insert(label);
                            }

                            proto::build::warp::requirement::Requirement::Dependency(dep_req) => {
                                let url = self
                                    .dependency_manager
                                    .find_by_package_name(&dep_req.name)
                                    .map(|dep| dep.url)
                                    .unwrap_or_else(|| dep_req.url.parse().unwrap());
                                let label: Label = url.into();
                                debug!("DEP: {} -> {}", &dep_req.name, label.to_string());
                                deps.insert(label);
                            }

                            proto::build::warp::requirement::Requirement::File(file_req) => {
                                let label = code_db
                                    .find_label_for_file(&file_req.path)
                                    .await
                                    .map_err(SignatureStoreError::CodeDbError)?;
                                debug!("DEP: {} -> {}", &file_req.path, label.to_string());
                                deps.insert(label);
                            }

                            proto::build::warp::requirement::Requirement::Symbol(sym_req) => {
                                let label = code_db
                                    .find_label_for_symbol(&sym_req.raw, &sym_req.kind)
                                    .await
                                    .map_err(SignatureStoreError::CodeDbError)?;
                                debug!(
                                    "DEP: {}:{} -> {}",
                                    &sym_req.raw,
                                    &sym_req.kind,
                                    label.to_string()
                                );
                                deps.insert(label);
                            }
                        }
                    }

                    let runtime_deps: DashSet<Label> = DashSet::new();
                    for dep in sig.runtime_deps {
                        let req = dep.requirement.unwrap();
                        match req {
                            proto::build::warp::requirement::Requirement::Url(url_req) => {
                                let url: url::Url = url_req.url.parse().unwrap();
                                let label: Label = url.clone().into();
                                debug!("DEP: {} -> {}", &url, label.to_string());
                                runtime_deps.insert(label);
                            }

                            proto::build::warp::requirement::Requirement::Dependency(dep_req) => {
                                let url = self
                                    .dependency_manager
                                    .find_by_package_name(&dep_req.name)
                                    .map(|dep| dep.url)
                                    .unwrap_or_else(|| dep_req.url.parse().unwrap());
                                let label: Label = url.into();
                                debug!("DEP: {} -> {}", &dep_req.name, label.to_string());
                                runtime_deps.insert(label);
                            }

                            proto::build::warp::requirement::Requirement::File(file_req) => {
                                let label = code_db
                                    .find_label_for_file(&file_req.path)
                                    .await
                                    .map_err(SignatureStoreError::CodeDbError)?;
                                debug!("DEP: {} -> {}", &file_req.path, label.to_string());
                                runtime_deps.insert(label);
                            }

                            proto::build::warp::requirement::Requirement::Symbol(sym_req) => {
                                let label = code_db
                                    .find_label_for_symbol(&sym_req.raw, &sym_req.kind)
                                    .await
                                    .map_err(SignatureStoreError::CodeDbError)?;
                                debug!(
                                    "DEP: {}:{} -> {}",
                                    &sym_req.raw,
                                    &sym_req.kind,
                                    label.to_string()
                                );
                                runtime_deps.insert(label);
                            }
                        }
                    }

                    let signature = Signature {
                        name: sig.name.parse().unwrap(),
                        rule: sig.rule,
                        deps: deps.into_iter().collect(),
                        runtime_deps: runtime_deps.into_iter().collect(),
                        config: sig.config.map(|c| c.into()).unwrap_or_default(),
                    };

                    signatures.push(signature)
                }
            }
            // NOTE(@ostera): in the case where we try to analyze something but we are missing
            // dependencies for it, we want to use the dependency manager to find the right
            // label they point to, and surface that so we build those things before analyzing
            // again.
            proto::build::warp::codedb::generate_signature_response::Response::MissingDeps(
                proto::build::warp::codedb::GenerateSignatureMissingDepsResponse {
                    dependencies,
                    ..
                },
            ) => {
                println!("MISSING DEPS!");

                let mut dep_labels = vec![];
                let mut dep_ids = vec![];

                for dep in dependencies {
                    let req = dep.requirement.unwrap();
                    match req {
                        proto::build::warp::requirement::Requirement::Url(url_req) => {
                            let url: url::Url = url_req.url.parse().unwrap();
                            let label: Label = url.clone().into();
                            dep_labels.push(label.clone());
                            let dep_id = self.label_registry.register_label(label);
                            dep_ids.push(dep_id);
                        }

                        proto::build::warp::requirement::Requirement::Dependency(dep_req) => {
                            let url = self
                                .dependency_manager
                                .find_by_package_name(&dep_req.name)
                                .map(|dep| dep.url)
                                .unwrap_or_else(|| dep_req.url.parse().unwrap());
                            let label: Label = url.into();
                            dep_labels.push(label.clone());
                            let dep_id = self.label_registry.register_label(label);
                            dep_ids.push(dep_id);
                        }

                        proto::build::warp::requirement::Requirement::File(file_req) => {
                            let label = code_db
                                .find_label_for_file(&file_req.path)
                                .await
                                .map_err(SignatureStoreError::CodeDbError)?;
                            dep_labels.push(label.clone());
                            let dep_id = self.label_registry.register_label(label);
                            dep_ids.push(dep_id);
                        }

                        proto::build::warp::requirement::Requirement::Symbol(sym_req) => {
                            let label = code_db
                                .find_label_for_symbol(&sym_req.raw, &sym_req.kind)
                                .await
                                .map_err(SignatureStoreError::CodeDbError)?;
                            dep_labels.push(label.clone());
                            let dep_id = self.label_registry.register_label(label);
                            dep_ids.push(dep_id);
                        }
                    }
                }

                return Err(SignatureStoreError::MissingDeps {
                    label: label.clone(),
                    dep_ids,
                    dep_labels,
                });
            }
        }

        self.event_channel.send(Event::GeneratingSignature {
            label: label.to_owned(),
        });

        let gen_sig = GeneratedSignature { signatures };

        let json_gen_sig = serde_json::to_string_pretty(&gen_sig).unwrap();

        self._save(local_label, source_chunk, json_gen_sig.as_bytes())
            .await?;

        Ok(gen_sig)
    }

    async fn find_dep_paths(
        &self,
    ) -> Result<Vec<proto::build::warp::Dependency>, SignatureStoreError> {
        // NOTE(@ostera): we are getting all the paths to all the dependencies, since the
        // signature generators may need to look up certain files here.
        //
        let mut dependencies = vec![];
        for l in self.dependency_manager.labels().into_iter() {
            let label = self.label_registry.get_label(l);
            let store_path = label
                .workspace()
                .map(|p| p.to_path_buf())
                .unwrap_or_else(|| PathBuf::from(""))
                .join(label.path())
                .to_string_lossy()
                .to_string();

            if let Some(r) = self.build_results.get_build_result(l) {
                let store_path = self
                    .artifact_store
                    .absolute_path_by_node(&r.executable_target)
                    .await
                    .map_err(SignatureStoreError::ArtifactStoreError)?;

                dependencies.extend(r.target_manifest.outs.iter().map(|out| {
                    proto::build::warp::Dependency {
                        name: label.name().to_string(),
                        store_path: store_path.join(out).to_string_lossy().to_string(),
                        ..proto::build::warp::Dependency::default()
                    }
                }));
            };

            dependencies.push(proto::build::warp::Dependency {
                name: label.name().to_string(),
                store_path,
                ..proto::build::warp::Dependency::default()
            });
        }

        Ok(dependencies)
    }
}
