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

pub type SourceHash = String;
pub type AstHash = String;

#[derive(Default, Debug, Clone)]
pub struct SourceHasher;

#[derive(Error, Debug)]
pub enum SourceHasherError {
    #[error(transparent)]
    IOError(std::io::Error),
}

impl SourceHasher {
    pub async fn hash_source(file: &Path) -> Result<(String, SourceHash), SourceHasherError> {
        let mut f = fs::File::open(&file)
            .await
            .map_err(SourceHasherError::IOError)?;
        let mut buffer = Vec::with_capacity(2048);
        f.read_to_end(&mut buffer)
            .await
            .map_err(SourceHasherError::IOError)?;

        let mut s = Sha256::new();
        s.update(&buffer);
        Ok((
            String::from_utf8_lossy(&buffer).to_string(),
            format!("{:x}", s.finalize()),
        ))
    }
}

#[derive(Default, Debug, Clone, Hash, PartialOrd, Ord, PartialEq, Eq, Serialize, Deserialize)]
pub struct ParserResult {
    version: usize,
    ast: String,
}

impl ParserResult {
    fn hash(&self) -> AstHash {
        let mut s = Sha256::new();
        s.update(&self.ast);
        format!("{:x}", s.finalize())
    }
}

#[derive(Default, Debug, Clone, Hash, Serialize, Deserialize)]
pub struct SourceFile {
    pub source: String,
    pub symbol: SourceSymbol,
    pub ast_hash: AstHash,
    pub source_hash: SourceHash,
}

#[derive(Default, Debug, Clone, Hash, Eq, PartialEq, Ord, PartialOrd, Serialize, Deserialize)]
pub enum SourceSymbol {
    #[default]
    All,
    Named(String),
}

impl SourceSymbol {
    pub fn from_label_and_goal(label: &Label, goal: Goal) -> Self {
        match goal {
            Goal::Build => Self::All,
            Goal::Run => Self::All,
            Goal::Test => {
                let name = label.name();
                Self::Named(name.to_string())
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
    artifact_store: Arc<ArtifactStore>,
    build_results: Arc<BuildResults>,
    event_channel: Arc<EventChannel>,
    global_signatures_path: PathBuf,
    label_registry: Arc<LabelRegistry>,
    parsers: DashMap<String, LabelId>,
    sources: DashMap<(LabelId, SourceHash, SourceSymbol), SourceFile>,
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
}

impl SourceManager {
    pub fn new(
        workspace: &Workspace,
        build_results: Arc<BuildResults>,
        event_channel: Arc<EventChannel>,
        artifact_store: Arc<ArtifactStore>,
        label_registry: Arc<LabelRegistry>,
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
            artifact_store,
            build_results,
            event_channel,
            label_registry,
            parsers,
            sources: DashMap::new(),
            global_signatures_path: workspace.paths.global_signatures_path.clone(),
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
        let (contents, source_hash) = SourceHasher::hash_source(path)
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

        if let Some(parser) = self.parsers.get(&*local_label.extension()) {
            let source_file = self
                .dump_ast(local_label, *parser, contents, source_hash, symbol.clone())
                .await?;
            self._save(&source_key, &source_file).await?;
            self.sources.insert(fast_source_key, source_file.clone());
            return Ok(source_file);
        }

        Err(SourceManagerError::UnknownParser {
            label: label.clone(),
        })
    }

    async fn dump_tree_sitter_ast(
        &self,
        label: &LocalLabel,
        source: String,
        source_hash: SourceHash,
    ) -> Result<SourceFile, SourceManagerError> {
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(match &*label.extension() {
                "hrl" | "erl" => tree_sitter_erlang::language(),
                "go" => tree_sitter_go::language(),
                "js" => tree_sitter_javascript::language(),
                "py" => tree_sitter_python::language(),
                "rs" => tree_sitter_rust::language(),
                "swift" => tree_sitter_swift::language(),
                "ts" => tree_sitter_typescript::language_typescript(),
                "jsx" | "tsx" => tree_sitter_typescript::language_tsx(),
                _ => {
                    return Err(SourceManagerError::UnknownParser {
                        label: label.clone().into(),
                    })
                }
            })
            .unwrap();

        self.event_channel.send(Event::AnalyzingSource {
            label: label.to_owned().into(),
        });

        let tree = parser.parse(&source, None).unwrap();

        let parser_result = ParserResult {
            version: 0,
            ast: tree.root_node().to_sexp(),
        };

        Ok(SourceFile {
            symbol: SourceSymbol::All,
            ast_hash: parser_result.hash(),
            source_hash,
            source,
        })
    }

    async fn dump_ast(
        &self,
        label: &LocalLabel,
        parser_id: LabelId,
        source: String,
        source_hash: SourceHash,
        symbol: SourceSymbol,
    ) -> Result<SourceFile, SourceManagerError> {
        let parser = self
            .build_results
            .get_build_result(parser_id)
            .ok_or_else(|| SourceManagerError::MissingParser {
                parser_id,
                label: label.to_owned().into(),
                parser: self.label_registry.get_label(parser_id).as_ref().to_owned(),
            })?;

        self.event_channel.send(Event::AnalyzingSource {
            label: label.to_owned().into(),
        });

        let cmd = CommandRunner::builder()
            .cwd(PathBuf::from("."))
            .manifest(parser.target_manifest)
            .target(parser.executable_target)
            .args(vec![
                "dump-ast".to_string(),
                symbol.to_string(),
                label.file().to_string_lossy().to_string(),
            ])
            .sandboxed(false)
            .stream_outputs(false)
            .build()
            .map_err(SourceManagerError::CommandRunnerError)?;

        let result = cmd
            .run()
            .await
            .map_err(SourceManagerError::CommandRunnerError)?;

        if result.status != 0 {
            return Err(SourceManagerError::AnalyzerError {
                stdout: result.stdout,
                stderr: result.stderr,
                status: result.status,
                label: label.to_owned().into(),
            });
        }

        let parser_result: ParserResult = serde_json::from_slice(result.stdout.as_bytes())
            .map_err(|err| SourceManagerError::ParseError {
                label: label.to_owned().into(),
                parser: self.label_registry.get_label(parser_id).as_ref().to_owned(),
                err,
                ast: result.stdout.clone(),
            })?;

        Ok(SourceFile {
            symbol,
            ast_hash: parser_result.hash(),
            source_hash,
            source,
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
                label.name().unwrap(),
                symbol.to_string(),
            ))
            .with_extension("ast")
    }
}
