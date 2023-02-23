use super::*;
use dashmap::DashMap;
use serde::Deserialize;
use serde::Serialize;
use sha2::Digest;
use sha2::Sha256;
use std::fmt::Display;
use std::path::Path;
use std::path::PathBuf;
use crate::sync::Arc;
use thiserror::*;
use tokio::fs;
use tokio::io::AsyncReadExt;

#[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub enum SourceInput {
    Path(PathBuf),
    Chunk(SourceFile),
}

impl SourceInput {
    pub fn path(&self) -> PathBuf {
        match self {
            SourceInput::Path(p) => p.to_owned(),
            SourceInput::Chunk(src_file) => src_file.path.clone(),
        }
    }
}

pub type SourceHash = String;
pub type AstHash = String;

#[derive(Default, Debug, Clone)]
pub struct SourceHasher;

#[derive(Error, Debug)]
pub enum SourceHasherError {
    #[error("Source hasher could not read source file at {path:?} due to {err:?}")]
    CouldNotReadSource { path: PathBuf, err: std::io::Error },

    #[error("Source hasher could not open source file at {path:?} due to {err:?}")]
    CouldNotOpenSource { path: PathBuf, err: std::io::Error },
}

impl SourceHasher {
    pub async fn hash_source(file: &Path) -> Result<(String, SourceHash), SourceHasherError> {
        let mut f =
            fs::File::open(&file)
                .await
                .map_err(|err| SourceHasherError::CouldNotOpenSource {
                    path: file.into(),
                    err,
                })?;
        let mut buffer = Vec::with_capacity(2048);
        f.read_to_end(&mut buffer)
            .await
            .map_err(|err| SourceHasherError::CouldNotReadSource {
                path: file.into(),
                err,
            })?;

        let mut s = Sha256::new();
        s.update(&buffer);
        Ok((
            String::from_utf8_lossy(&buffer).to_string(),
            format!("{:x}", s.finalize()),
        ))
    }
}

#[derive(Default, Debug, Clone, Hash, Serialize, Deserialize, Ord, Eq, PartialEq, PartialOrd)]
pub struct SourceFile {
    pub source: String,
    pub symbol: SourceSymbol,
    pub ast_hash: AstHash,
    pub source_hash: SourceHash,
    pub path: PathBuf,
}

#[derive(Default, Debug, Clone, Hash, Eq, PartialEq, Ord, PartialOrd, Serialize, Deserialize)]
pub enum SourceSymbol {
    #[default]
    All,
    Named(String),
}

impl SourceSymbol {
    pub fn is_named(&self) -> bool {
        matches!(&self, Self::Named(_))
    }

    pub fn is_all(&self) -> bool {
        matches!(&self, Self::All)
    }

    #[tracing::instrument(name = "SourceSymbol::from_label_and_goal")]
    pub fn from_label_and_goal(label: &Label, goal: Goal) -> Self {
        match goal {
            Goal::Build => Self::All,
            Goal::Run => Self::All,
            Goal::Fetch => Self::All,
            Goal::Test => {
                let name = label.name();
                let file_name = label.get_local().unwrap().filename();
                if name == file_name {
                    Self::All
                } else {
                    Self::Named(name.to_string())
                }
            }
        }
    }
}

impl ToString for SourceSymbol {
    fn to_string(&self) -> String {
        match &self {
            SourceSymbol::All => "@all".to_string(),
            SourceSymbol::Named(name) => name.to_string(),
        }
    }
}

impl From<SourceSymbol> for proto::build::warp::Symbol {
    fn from(symbol: SourceSymbol) -> Self {
        proto::build::warp::Symbol {
            sym: Some(match symbol {
                SourceSymbol::All => proto::build::warp::symbol::Sym::All(true),
                SourceSymbol::Named(name) => {
                    proto::build::warp::symbol::Sym::Named(name.to_string())
                }
            }),
        }
    }
}

/// A unique identifier for a source. It can only be constructed via `LabelRegistry::register`.
///
#[derive(Copy, Default, Debug, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct SourceId(u128);

impl Display for SourceId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(Clone, Debug)]
pub struct SourceManager {
    analyzer_service_manager: Arc<AnalyzerServiceManager>,
    dependency_manager: Arc<DependencyManager>,
    global_signatures_path: PathBuf,
    label_registry: Arc<LabelRegistry>,
    sources: DashMap<(LabelId, SourceHash, SourceSymbol), SourceFile>,
    workspace: Workspace,
}

#[derive(Error, Debug)]
pub enum SourceManagerError {
    #[error("Label does not point to a file")]
    InvalidLabel { label_id: LabelId, label: Label },

    #[error("We don't know how to parse sources with this extension: {}", .label.to_string())]
    UnknownParser { label: Label },

    #[error("Could not parse {} with parser {}, due to: {err:?}. Full output:\n{ast}", .label.to_string(), .parser.to_string())]
    ParseError {
        label: Label,
        parser: Label,
        err: serde_json::Error,
        ast: String,
    },

    #[error(transparent)]
    HasherError(SourceHasherError),

    #[error(transparent)]
    CommandRunnerError(CommandRunnerError),

    #[error("Somehow we built a signature that we can't use")]
    BadSignature,

    #[error("Could not write AST file at {file:?} due to {err:?}")]
    AstWriteError { file: PathBuf, err: std::io::Error },

    #[error("Could not read AST file at {file:?} due to {err:?}")]
    AstReadError { file: PathBuf, err: std::io::Error },

    #[error("Could not parse AST file at {file:?} due to {err:?}. Full AST:\n{ast}")]
    AstParseError {
        file: PathBuf,
        err: serde_json::Error,
        ast: String,
    },

    #[error("Failed to analyze sources from {}. Analyzer exited with status {status}.\n\nStdout: {stdout}\n\nStderr: {stderr}", label.to_string())]
    AnalyzerError {
        stdout: String,
        stderr: String,
        status: i32,
        label: Label,
    },

    #[error(transparent)]
    AnalyzerServiceManagerError(AnalyzerServiceManagerError),

    #[error(transparent)]
    AnalyzerServiceError(tonic::Status),

    #[error("Could not chunk source for {} due to missing dependencies: {dep_labels:?}", .label.to_string())]
    MissingDeps {
        label: Label,
        dep_ids: Vec<LabelId>,
        dep_labels: Vec<Label>,
    },

    #[error(transparent)]
    CodeDbError(CodeDbError),
}

impl SourceManager {
    pub fn new(
        workspace: &Workspace,
        _build_results: Arc<BuildResults>,
        _event_channel: Arc<EventChannel>,
        _artifact_store: Arc<ArtifactStore>,
        label_registry: Arc<LabelRegistry>,
        analyzer_service_manager: Arc<AnalyzerServiceManager>,
        dependency_manager: Arc<DependencyManager>,
        _build_opts: BuildOpts,
    ) -> Self {
        Self {
            workspace: workspace.clone(),
            analyzer_service_manager,
            dependency_manager,
            global_signatures_path: workspace.paths.global_signatures_path.clone(),
            label_registry,
            sources: DashMap::new(),
        }
    }

    /// To get a chunk of a source file using a symbol (like the name of a test, or the 'all' symbol
    /// which means "give me the entire file"), we need to
    #[tracing::instrument(name = "SourceManager::get_source_chunk_by_symbol", skip(self))]
    pub async fn get_source_chunk_by_symbol(
        &self,
        label_id: LabelId,
        label: &Label,
        symbol: &SourceSymbol,
    ) -> Result<SourceFile, SourceManagerError> {
        let local_label = label.get_local().unwrap();

        let path = local_label.file();
        let (_source_contents, source_hash) = SourceHasher::hash_source(path)
            .await
            .map_err(SourceManagerError::HasherError)?;

        let fast_source_key = (label_id, source_hash.clone(), symbol.to_owned());
        if let Some(source_file) = self.sources.get(&fast_source_key) {
            return Ok(source_file.to_owned());
        }

        let source_key = (local_label, source_hash.clone(), symbol.to_owned());
        let ast_path = self.ast_path(&source_key);
        if let Ok(mut file) = fs::File::open(&ast_path).await {
            let mut bytes = vec![];
            file.read_to_end(&mut bytes)
                .await
                .map_err(|err| SourceManagerError::AstReadError {
                    file: ast_path.clone(),
                    err,
                })?;

            let source_file: SourceFile = serde_json::from_slice(&bytes).map_err(|err| {
                SourceManagerError::AstParseError {
                    file: ast_path.clone(),
                    err,
                    ast: String::from_utf8(bytes).unwrap(),
                }
            })?;

            self.sources.insert(fast_source_key, source_file.clone());

            return Ok(source_file);
        };

        if let Some(analyzer_id) = self
            .analyzer_service_manager
            .find_analyzer_for_local_label(local_label)
        {
            let mut analyzer_svc = self
                .analyzer_service_manager
                .start(analyzer_id)
                .await
                .map_err(SourceManagerError::AnalyzerServiceManagerError)?;

            // NOTE(@ostera): we are getting all the paths to all the dependencies, since the
            // signature generators may need to look up certain files here.
            //
            let dependencies = self
                .dependency_manager
                .labels()
                .into_iter()
                .map(|l| {
                    let label = self.label_registry.get_label(l);
                    let store_path = label
                        .workspace()
                        .map(|p| p.to_path_buf())
                        .unwrap_or_else(|| PathBuf::from(""))
                        .join(label.path())
                        .to_string_lossy()
                        .to_string();

                    proto::build::warp::Dependency {
                        name: label.name().to_string(),
                        store_path,
                        ..proto::build::warp::Dependency::default()
                    }
                })
                .collect();

            let request = proto::build::warp::codedb::GetAstRequest {
                file: path.to_string_lossy().to_string(),
                symbol: Some(symbol.to_owned().into()),
                dependencies,
            };

            let response = analyzer_svc
                .get_ast(request)
                .await
                .map_err(SourceManagerError::AnalyzerServiceError)?
                .into_inner()
                .response
                .unwrap();

            let code_db = CodeDb::new(&self.workspace).await.unwrap();

            match response {
                proto::build::warp::codedb::get_ast_response::Response::Ok(response) => {
                    let resp_symbol = response.symbol.unwrap().sym.unwrap();

                    let source_file = SourceFile {
                        symbol: match resp_symbol {
                            proto::build::warp::symbol::Sym::All(_) => SourceSymbol::All,
                            proto::build::warp::symbol::Sym::Named(name) => {
                                SourceSymbol::Named(name)
                            }
                        },
                        ast_hash: {
                            let mut s = Sha256::new();
                            s.update(&response.source);
                            format!("{:x}", s.finalize())
                        },
                        source_hash,
                        source: response.source,
                        path: path.to_path_buf(),
                    };

                    self._save(&source_key, &source_file).await?;
                    self.sources.insert(fast_source_key, source_file.clone());
                    return Ok(source_file);
                }

                // NOTE(@ostera): in the case where we try to analyze something but we are missing
                // dependencies for it, we want to use the dependency manager to find the right
                // label they point to, and surface that so we build those things before analyzing
                // again.
                proto::build::warp::codedb::get_ast_response::Response::MissingDeps(
                    proto::build::warp::codedb::GetAstMissingDepsResponse { dependencies, .. },
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
                                    .map_err(SourceManagerError::CodeDbError)?;
                                dep_labels.push(label.clone());
                                let dep_id = self.label_registry.register_label(label);
                                dep_ids.push(dep_id);
                            }

                            proto::build::warp::requirement::Requirement::Symbol(sym_req) => {
                                let label = code_db
                                    .find_label_for_symbol(&sym_req.raw, &sym_req.kind)
                                    .await
                                    .map_err(SourceManagerError::CodeDbError)?;
                                dep_labels.push(label.clone());
                                let dep_id = self.label_registry.register_label(label);
                                dep_ids.push(dep_id);
                            }
                        }
                    }

                    return Err(SourceManagerError::MissingDeps {
                        label: label.clone(),
                        dep_ids,
                        dep_labels,
                    });
                }
            };
        }

        Err(SourceManagerError::UnknownParser {
            label: label.clone(),
        })
    }

    pub async fn save(
        &self,
        local_label: &LocalLabel,
        source_file: &SourceFile,
    ) -> Result<(), SourceManagerError> {
        let (_, hash) = SourceHasher::hash_source(local_label.file())
            .await
            .map_err(SourceManagerError::HasherError)?;

        self._save(
            &(local_label, hash, source_file.symbol.clone()),
            source_file,
        )
        .await
    }

    async fn _save(
        &self,
        source_key: &(&LocalLabel, SourceHash, SourceSymbol),
        source_file: &SourceFile,
    ) -> Result<(), SourceManagerError> {
        let ast_path = self.ast_path(source_key);

        let json = serde_json::to_string_pretty(&source_file).unwrap();

        fs::write(&ast_path, json)
            .await
            .map_err(|err| SourceManagerError::AstWriteError {
                file: ast_path.clone(),
                err,
            })
    }

    fn ast_path(
        &self,
        (label, source_hash, symbol): &(&LocalLabel, SourceHash, SourceSymbol),
    ) -> PathBuf {
        self.global_signatures_path
            .join(format!(
                "{:x}-{}-{}-{}",
                label.hash(),
                source_hash,
                label.file().file_name().unwrap().to_string_lossy(),
                symbol.to_string().replace('/', "_"),
            ))
            .with_extension("ast")
    }
}
