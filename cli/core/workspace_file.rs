use super::*;
use anyhow::*;
use serde_derive::{Deserialize, Serialize};
use std::path::Path;
use thiserror::Error;
use tokio::fs;
use tokio::io::AsyncReadExt;
use tracing::*;
use url::Url;

pub const WORKSPACE: &str = "Workspace.toml";

pub const DEFAULT_IGNORE: [&str; 1] = ["warp-outputs"];

#[derive(Error, Debug)]
pub enum WorkspaceFileError {
    #[error("Could not parse Workspace file: {0:?}")]
    ParseError(toml::de::Error),

    #[error("Could not print Workspace file: {0:#?}")]
    PrintError(toml::ser::Error),

    #[error(transparent)]
    IOError(std::io::Error),
}

/// A struct representing the top-level workspace configuration of a `Workspace.toml` file in a
/// Warp Workspace.
///
#[derive(Clone, Default, Debug, Builder, Serialize, Deserialize)]
#[builder(build_fn(error = "anyhow::Error"))]
pub struct WorkspaceConfig {
    pub name: String,

    #[builder(default)]
    pub remote_cache_url: Option<Url>,

    #[builder(default)]
    pub ignore_patterns: Vec<String>,

    #[builder(default)]
    pub use_git_hooks: bool,
}

impl WorkspaceConfig {
    pub fn builder() -> WorkspaceConfigBuilder {
        WorkspaceConfigBuilder::default()
    }
}

/// A struct representing a `Workspace.toml` file in a Warp Workspace.
///
/// This is primarily used for serialization/deserialization, and manipualting the file itself
/// through a semantic API that hides the TOML disk representation.
///
#[derive(Clone, Default, Debug, Builder, Serialize, Deserialize)]
#[builder(build_fn(error = "anyhow::Error"))]
pub struct WorkspaceFile {
    #[builder(default)]
    pub workspace: WorkspaceConfig,

    #[builder(default)]
    pub aliases: std::collections::HashMap<String, Label>,

    // NOTE(@ostera): in _this case_ we don't know enough about the shape of the toolchain
    // configuration to do any validation, and we haven't yet spinned up the RuleExecEnv to find
    // out, so we are stuck with saving whatever TOML the user typed in :( and validating later
    #[builder(default)]
    pub toolchains: std::collections::HashMap<String, toml::Value>,
}

impl WorkspaceFile {
    pub fn builder() -> WorkspaceFileBuilder {
        WorkspaceFileBuilder::default()
    }

    pub async fn read_from_file(&self, path: &Path) -> Result<(), WorkspaceFileError> {
        let mut bytes = vec![];
        let mut file = fs::File::open(path)
            .await
            .map_err(WorkspaceFileError::IOError)?;
        file.read_to_end(&mut bytes)
            .await
            .map_err(WorkspaceFileError::IOError)?;
        toml::from_slice(&bytes).map_err(WorkspaceFileError::ParseError)
    }

    pub async fn write(&self, root: &Path) -> Result<(), WorkspaceFileError> {
        let toml = toml::to_string(&self).map_err(WorkspaceFileError::PrintError)?;
        fs::write(&root.join(WORKSPACE), toml)
            .await
            .map_err(WorkspaceFileError::IOError)
    }
}
