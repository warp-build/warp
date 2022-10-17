use super::*;
use futures::StreamExt;
use serde_derive::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use thiserror::*;
use tokio::fs;
use tokio::io::AsyncReadExt;
use tracing::*;

pub const WORKSPACE: &str = "Workspace.json";

#[derive(Error, Debug)]
pub enum RemoteWorkspaceFileError {
    #[error(
        r#"The configuration for a remote workspace needs to have either:

1. A `url` and a `sha1` field, or
2. A `github` and a `ref` field

Instead we found: {0:?}"#
    )]
    BadConfig(RemoteWorkspaceFile),

    #[error(r#"Key 'github' in remote workspace configuration must have form 'username/repo', instead we found: {0}"#)]
    MalformedGithubString(String),
}

#[derive(Error, Debug)]
pub enum WorkspaceFileError {
    #[error("Could not parse Workspace file: {0:?}")]
    ParseError(serde_json::Error),

    #[error("Could not print Workspace file: {0:#?}")]
    PrintError(serde_json::Error),

    #[error(transparent)]
    IOError(std::io::Error),

    #[error(transparent)]
    RemoteWorkspaceError(RemoteWorkspaceFileError),

    #[error("Could not find workspace a file walking upwards your file system. Are you sure we're in the right place?")]
    WorkspaceFileNotFound,

    #[error("Attempted to build a Workspace while missing fields: {0:?}")]
    BuilderError(derive_builder::UninitializedFieldError),

    #[error("{0}")]
    ValidationError(String),
}

impl From<derive_builder::UninitializedFieldError> for WorkspaceFileError {
    fn from(err: derive_builder::UninitializedFieldError) -> Self {
        Self::BuilderError(err)
    }
}

impl From<String> for WorkspaceFileError {
    fn from(s: String) -> Self {
        Self::ValidationError(s)
    }
}

/// A struct representing the top-level workspace configuration of a `Workspace.json` file in a
/// Warp Workspace.
///
#[derive(Clone, Default, Debug, Builder, Serialize, Deserialize)]
#[builder(build_fn(error = "WorkspaceFileError"))]
pub struct WorkspaceConfig {
    pub name: String,

    #[builder(default = "vec![]")]
    #[serde(default)]
    pub ignore_patterns: Vec<String>,

    #[builder(default = "false")]
    #[serde(default)]
    pub use_git_hooks: bool,

    #[builder(default)]
    #[serde(default)]
    pub remote_cache_url: Option<String>,
}

impl WorkspaceConfig {
    pub fn builder() -> WorkspaceConfigBuilder {
        WorkspaceConfigBuilder::default()
    }
}

#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub struct RemoteWorkspaceFile {
    #[serde(default, alias = "url")]
    pub archive_url: Option<url::Url>,

    #[serde(default, alias = "sha1")]
    pub archive_sha1: Option<String>,

    #[serde(default, alias = "prefix")]
    pub archive_prefix: Option<String>,

    #[serde(default)]
    pub github: Option<String>,

    #[serde(default)]
    pub git_ref: Option<String>,
}
#[derive(Clone, Default, Debug, Serialize, Deserialize)]
pub struct FlexibleRuleConfig(pub BTreeMap<String, toml::Value>);

/// A struct representing a `Workspace.json` file in a Warp Workspace.
///
/// This is primarily used for serialization/deserialization, and manipualting the file itself
/// through a semantic API that hides the json disk representation.
///
#[derive(Clone, Default, Debug, Builder, Serialize, Deserialize)]
#[builder(build_fn(error = "WorkspaceFileError"))]
pub struct WorkspaceFile {
    pub workspace: WorkspaceConfig,

    #[builder(default)]
    #[serde(default)]
    pub aliases: BTreeMap<String, String>,

    #[builder(default)]
    #[serde(default)]
    pub toolchains: BTreeMap<String, FlexibleRuleConfig>,

    #[builder(default)]
    #[serde(default)]
    pub remote_workspaces: BTreeMap<String, RemoteWorkspaceFile>,
}

impl WorkspaceFile {
    #[tracing::instrument(name = "WorkspaceFile::walk_uptree")]
    async fn walk_uptree(start: PathBuf) -> impl futures::Stream<Item = PathBuf> {
        let mut cwd = start;
        async_stream::stream! {
            yield cwd.clone();
            while let Some(parent) = cwd.parent() {
                cwd = parent.to_path_buf();
                yield cwd.clone();
            }
        }
    }

    #[tracing::instrument(name = "WorkspaceFile::find_upwards")]
    pub async fn find_upwards(cwd: &Path) -> Result<(PathBuf, Self), WorkspaceFileError> {
        let mut dirs = Box::pin(WorkspaceFile::walk_uptree(cwd.to_path_buf()).await);
        while let Some(path) = dirs.next().await {
            let here = &path.join(WORKSPACE);
            if fs::canonicalize(&here).await.is_ok() {
                return WorkspaceFile::read_from_file(here)
                    .await
                    .map(|file| (path.clone(), file));
            }
        }
        Err(WorkspaceFileError::WorkspaceFileNotFound)
    }

    pub fn builder() -> WorkspaceFileBuilder {
        WorkspaceFileBuilder::default()
    }

    #[tracing::instrument(name = "WorkspaceFile::read_from_file")]
    pub async fn read_from_file(path: &Path) -> Result<Self, WorkspaceFileError> {
        let mut bytes = vec![];

        let mut file = fs::File::open(path)
            .await
            .map_err(WorkspaceFileError::IOError)?;

        file.read_to_end(&mut bytes)
            .await
            .map_err(WorkspaceFileError::IOError)?;

        serde_json::from_slice(&bytes).map_err(WorkspaceFileError::ParseError)
    }

    #[tracing::instrument(name = "WorkspaceFile::write")]
    pub async fn write(&self, root: &Path) -> Result<(), WorkspaceFileError> {
        let json = serde_json::to_string_pretty(&self).map_err(WorkspaceFileError::PrintError)?;
        fs::write(&root.join(WORKSPACE), json)
            .await
            .map_err(WorkspaceFileError::IOError)
    }
}

impl TryFrom<&Workspace> for WorkspaceFile {
    type Error = WorkspaceFileError;

    fn try_from(w: &Workspace) -> Result<Self, WorkspaceFileError> {
        Self::builder()
            .workspace(
                WorkspaceConfig::builder()
                    .name(w.name.clone())
                    .ignore_patterns(w.ignore_patterns.clone())
                    .use_git_hooks(w.use_git_hooks)
                    .build()?,
            )
            .aliases(
                w.aliases
                    .iter()
                    .map(|(k, v)| (k.clone(), v.to_string()))
                    .collect(),
            )
            .remote_workspaces(
                w.remote_workspace_configs
                    .iter()
                    .map(|(k, v)| (k.clone(), v.clone().into()))
                    .collect(),
            )
            .build()
    }
}
