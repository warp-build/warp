use super::*;
use dashmap::DashMap;
use serde::Deserialize;
use serde::Serialize;
use sha2::Digest;
use sha2::Sha256;
use std::fmt::Display;
use std::path::Path;
use std::path::PathBuf;
use std::sync::Arc;
use thiserror::*;
use tokio::fs;
use tokio::io::AsyncReadExt;
use url::Url;

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
    #[error("Could not read source file at {path:?} due to {err:?}")]
    CouldNotReadSource { path: PathBuf, err: std::io::Error },

    #[error("Could not open source file at {path:?} due to {err:?}")]
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

    artifact_store: Arc<ArtifactStore>,

    build_results: Arc<BuildResults>,

    event_channel: Arc<EventChannel>,

    global_signatures_path: PathBuf,

    label_registry: Arc<LabelRegistry>,

    // TODO(@ostera): remove this! we should be
    parsers: DashMap<String, LabelId>,

    sources: DashMap<(LabelId, SourceHash, SourceSymbol), SourceFile>,
    build_opts: BuildOpts,
}

#[derive(Error, Debug)]
pub enum SourceManagerError {
    #[error("Label does not point to a file")]
    InvalidLabel { label_id: LabelId, label: Label },

    #[error("We don't know how to parse sources with this extension: {}", .label.to_string())]
    UnknownParser { label: Label },

    #[error("Parser {} needs to be built before parsing {}", .parser.to_string(), .label.to_string())]
    MissingParser {
        parser_id: LabelId,
        parser: Label,
        label: Label,
    },

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
}

impl SourceManager {
    pub fn new(
        workspace: &Workspace,
        build_results: Arc<BuildResults>,
        event_channel: Arc<EventChannel>,
        artifact_store: Arc<ArtifactStore>,
        label_registry: Arc<LabelRegistry>,
        analyzer_service_manager: Arc<AnalyzerServiceManager>,
        build_opts: BuildOpts,
    ) -> Self {
        let parsers = DashMap::new();

        for (ext, parser) in &[
            ("erl", "https://tools.warp.build/erlang/lifter"),
            ("hrl", "https://tools.warp.build/erlang/lifter"),
            ("go", "https://tools.warp.build/tree-sitter/parser"),
            ("js", "https://tools.warp.build/tree-sitter/parser"),
            ("py", "https://tools.warp.build/tree-sitter/parser"),
            ("rs", "https://tools.warp.build/tree-sitter/parser"),
            ("swift", "https://tools.warp.build/tree-sitter/parser"),
            ("ts", "https://tools.warp.build/tree-sitter/parser"),
            ("tsx", "https://tools.warp.build/tree-sitter/parser"),
            ("jsx", "https://tools.warp.build/tree-sitter/parser"),
        ] {
            let label: Label = Url::parse(parser).unwrap().into();
            let label_id = label_registry.register_label(label);
            parsers.insert(ext.to_string(), label_id);
        }

        Self {
            analyzer_service_manager,
            artifact_store,
            build_results,
            event_channel,
            global_signatures_path: workspace.paths.global_signatures_path.clone(),
            label_registry,
            parsers,
            sources: DashMap::new(),
            build_opts,
        }
    }

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

            let request = proto::build::warp::codedb::GetAstRequest {
                file: path.to_string_lossy().to_string(),
                symbol: Some(proto::build::warp::Symbol {
                    sym: Some(match symbol {
                        SourceSymbol::All => proto::build::warp::symbol::Sym::All(true),
                        SourceSymbol::Named(name) => {
                            proto::build::warp::symbol::Sym::Named(name.to_string())
                        }
                    }),
                }),
            };

            let response = analyzer_svc
                .get_ast(request)
                .await
                .map_err(SourceManagerError::AnalyzerServiceError)?
                .into_inner();

            let resp_symbol = response.symbol.unwrap().sym.unwrap();

            let source_file = SourceFile {
                symbol: match resp_symbol {
                    proto::build::warp::symbol::Sym::All(_) => SourceSymbol::All,
                    proto::build::warp::symbol::Sym::Named(name) => SourceSymbol::Named(name),
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
