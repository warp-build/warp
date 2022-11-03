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
            String::from_utf8(buffer).unwrap(),
            format!("{:x}", s.finalize()),
        ))
    }
}

#[derive(Default, Debug, Clone, Hash, PartialOrd, Ord, PartialEq, Eq, Serialize, Deserialize)]
pub struct ParserResult {
    version: usize,
    sexpr: String,
}

impl ParserResult {
    fn hash(&self) -> String {
        let mut s = Sha256::new();
        s.update(&self.sexpr);
        format!("{:x}", s.finalize())
    }
}

#[derive(Default, Debug, Clone, Hash)]
pub struct SourceFile {
    pub source: String,
    pub hash: SourceHash,
    pub symbol: SourceSymbol,
}

#[derive(Default, Debug, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub enum SourceSymbol {
    #[default]
    All,
    Test(String),
}

impl SourceSymbol {
    pub fn from_label_and_goal(label_id: LabelId, label: &Label, goal: Goal) -> Self {
        Self::All
    }
}

impl ToString for SourceSymbol {
    fn to_string(&self) -> String {
        match &self {
            SourceSymbol::All => "all".to_string(),
            SourceSymbol::Test(name) => format!("test={}", name),
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
}

impl SourceManager {
    pub fn new(
        build_results: Arc<BuildResults>,
        event_channel: Arc<EventChannel>,
        artifact_store: Arc<ArtifactStore>,
        label_registry: Arc<LabelRegistry>,
    ) -> Self {
        let parsers = DashMap::new();

        for (ext, parser) in &[
            ("erl", "https://tools.warp.build/tree-sitter/parser"),
            ("hrl", "https://tools.warp.build/tree-sitter/parser"),
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
        let (contents, hash) = SourceHasher::hash_source(path)
            .await
            .map_err(SourceManagerError::HasherError)?;

        let source_key = (label_id, hash, symbol.to_owned());
        if let Some(source_file) = self.sources.get(&source_key) {
            return Ok(source_file.to_owned());
        }

        if let Some(parser) = self.parsers.get(&*local_label.extension()) {
            let source_file = match symbol {
                SourceSymbol::All => self.dump_ast(local_label, *parser, contents).await?,
                SourceSymbol::Test(name) => {
                    self.dump_subtree(local_label, *parser, contents, name, symbol.to_owned())
                        .await?
                }
            };
            self.sources.insert(source_key, source_file.clone());
            return Ok(source_file);
        }

        Err(SourceManagerError::UnknownParser {
            label: label.clone(),
        })
    }

    async fn dump_ast(
        &self,
        label: &LocalLabel,
        parser_id: LabelId,
        source: String,
    ) -> Result<SourceFile, SourceManagerError> {
        let parser = self
            .build_results
            .get_build_result(parser_id)
            .ok_or_else(|| SourceManagerError::MissingParser {
                parser_id,
                label: label.to_owned().into(),
                parser: self.label_registry.get_label(parser_id).as_ref().to_owned(),
            })?;

        self.event_channel.send(Event::GeneratingSignature {
            label: label.to_owned().into(),
        });

        let cmd = CommandRunner::builder()
            .cwd(PathBuf::from("."))
            .manifest(parser.target_manifest)
            .target(parser.executable_target)
            .args(vec![
                "dump-ast".to_string(),
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

        let parser_result: ParserResult = serde_json::from_slice(result.stdout.as_bytes())
            .map_err(|err| SourceManagerError::ParseError {
                label: label.to_owned().into(),
                parser: self.label_registry.get_label(parser_id).as_ref().to_owned(),
                err,
                ast: result.stdout.clone(),
            })?;

        Ok(SourceFile {
            symbol: SourceSymbol::All,
            hash: parser_result.hash(),
            source,
        })
    }

    async fn dump_subtree(
        &self,
        label: &LocalLabel,
        parser_id: LabelId,
        source: String,
        symbol_name: &str,
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

        self.event_channel.send(Event::GeneratingSignature {
            label: label.to_owned().into(),
        });

        let cmd = CommandRunner::builder()
            .cwd(PathBuf::from("."))
            .manifest(parser.target_manifest)
            .target(parser.executable_target)
            .args(vec![
                "dump-subtree".to_string(),
                label.file().to_string_lossy().to_string(),
                symbol_name.to_string(),
            ])
            .sandboxed(false)
            .stream_outputs(false)
            .build()
            .map_err(SourceManagerError::CommandRunnerError)?;

        let result = cmd
            .run()
            .await
            .map_err(SourceManagerError::CommandRunnerError)?;

        let parser_result: ParserResult = serde_json::from_slice(result.stdout.as_bytes())
            .map_err(|err| SourceManagerError::ParseError {
                label: label.to_owned().into(),
                parser: self.label_registry.get_label(parser_id).as_ref().to_owned(),
                err,
                ast: result.stdout.clone(),
            })?;

        Ok(SourceFile {
            symbol,
            hash: parser_result.hash(),
            source,
        })
    }
}
