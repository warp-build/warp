use super::*;
use anyhow::*;
use futures::StreamExt;
use serde_derive::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use thiserror::Error;
use tokio::fs;
use tokio::io::AsyncReadExt;
use tracing::*;

pub const WORKSPACE: &str = "Workspace.toml";

#[derive(Error, Debug)]
pub enum WorkspaceFileError {
    #[error("Could not parse Workspace file: {0:?}")]
    ParseError(toml::de::Error),

    #[error("Could not print Workspace file: {0:#?}")]
    PrintError(toml::ser::Error),

    #[error(transparent)]
    IOError(std::io::Error),

    #[error("Could not find workspace a file walking upwards your file system. Are you sure we're in the right place?")]
    WorkspaceFileNotFound,

    #[error("Attempted to build a Workspace while missing fields: {0:?}")]
    BuilderError(derive_builder::UninitializedFieldError),

    #[error("{0}")]
    ValidationError(String),

    #[error(transparent)]
    Unknown(anyhow::Error),
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

/// A struct representing the top-level workspace configuration of a `Workspace.toml` file in a
/// Warp Workspace.
///
#[derive(Clone, Default, Debug, Builder, Serialize, Deserialize)]
#[builder(build_fn(error = "anyhow::Error"))]
pub struct WorkspaceConfig {
    pub name: String,

    #[builder(default = "vec![]")]
    pub ignore_patterns: Vec<String>,

    #[builder(default = "false")]
    pub use_git_hooks: bool,
}

impl WorkspaceConfig {
    pub fn builder() -> WorkspaceConfigBuilder {
        WorkspaceConfigBuilder::default()
    }
}

#[derive(Clone, Default, Debug, Serialize, Deserialize)]
pub struct FlexibleRuleConfig(pub BTreeMap<String, toml::Value>);

/// A struct representing a `Workspace.toml` file in a Warp Workspace.
///
/// This is primarily used for serialization/deserialization, and manipualting the file itself
/// through a semantic API that hides the TOML disk representation.
///
#[derive(Clone, Default, Debug, Builder, Serialize, Deserialize)]
#[builder(build_fn(error = "anyhow::Error"))]
pub struct WorkspaceFile {
    pub workspace: WorkspaceConfig,

    #[builder(default)]
    pub aliases: BTreeMap<String, String>,

    #[builder(default)]
    pub toolchains: BTreeMap<String, FlexibleRuleConfig>,
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

    pub async fn find_upwards(cwd: &Path) -> Result<(PathBuf, Self), anyhow::Error> {
        let mut dirs = Box::pin(WorkspaceFile::walk_uptree(cwd.to_path_buf()).await);
        while let Some(path) = dirs.next().await {
            let here = &path.join(WORKSPACE);
            if fs::canonicalize(&here).await.is_ok() {
                let file = WorkspaceFile::read_from_file(here).await?;
                return Ok((path.clone(), file));
            }
        }
        Err(anyhow!("Could not find Workspace file"))
    }

    pub fn builder() -> WorkspaceFileBuilder {
        WorkspaceFileBuilder::default()
    }

    pub async fn read_from_file(path: &Path) -> Result<Self, WorkspaceFileError> {
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

impl TryFrom<&Workspace> for WorkspaceFile {
    type Error = anyhow::Error;

    fn try_from(w: &Workspace) -> Result<Self, anyhow::Error> {
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
            .toolchains(
                w.toolchain_configs
                    .iter()
                    .map(|t| (t.name.clone(), t.try_into().unwrap()))
                    .collect(),
            )
            .build()
    }
}

/*
#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    fn parse(toml: toml::Value, root: &PathBuf) -> Result<Workspace, anyhow::Error> {
        let root = std::fs::canonicalize(root).unwrap();
        let paths = WorkspacePaths::new(&root, None, None).unwrap();
        WorkspaceParser::from_toml(toml, paths)
    }

    #[test]
    fn demands_workspace_name() {
        let toml: toml::Value = r#"
[workspace]
[toolchains]
[dependencies]
        "#
        .parse::<toml::Value>()
        .unwrap();
        let workspace = parse(toml, &PathBuf::from("."));
        assert_eq!(true, workspace.is_err());
    }

    #[test]
    fn parses_toml_into_workspace_struct() {
        let toml: toml::Value = r#"
[workspace]
name = "tiny_lib"

[toolchains]

[dependencies]
        "#
        .parse::<toml::Value>()
        .unwrap();
        let workspace = parse(toml, &PathBuf::from(".")).unwrap();
        assert_eq!(workspace.name, "tiny_lib");
    }

    #[test]
    fn expects_ignore_patterns_to_be_an_array() {
        let toml: toml::Value = r#"
[workspace]
name = "tiny_lib"
ignore_patterns = {}

[toolchains]

[dependencies]
        "#
        .parse::<toml::Value>()
        .unwrap();

        assert!(parse(toml, &PathBuf::from(".")).is_err());
    }

    #[test]
    fn parses_ignore_patterns_into_workspace() {
        let toml: toml::Value = r#"
[workspace]
name = "tiny_lib"
ignore_patterns = ["node_modules"]

[toolchains]

[dependencies]
        "#
        .parse::<toml::Value>()
        .unwrap();
        let workspace = parse(toml, &PathBuf::from(".")).unwrap();
        assert_eq!(
            workspace.ignore_patterns,
            vec!["node_modules", "warp-outputs"]
        );
    }

    /*
    #[test]
    fn allows_for_custom_toolchains() {
        let toml: toml::Value = r#"
    [workspace]
    name = "tiny_lib"

    [toolchains]
    erlang = { archive_url = "official", prefix = "otp-prefix" }
    gleam = { archive_url = "https://github.com/forked/gleam", sha1 = "sha1-test" }

    [dependencies]
            "#
        .parse::<toml::Value>()
        .unwrap();
        let _workspace = parse(toml, &PathBuf::from(".")).unwrap();
        assert_eq!(
            "official",
            toolchain_manager.get_archive("erlang").unwrap().url()
        );
        assert_eq!(
            "otp-prefix",
            toolchain_manager.get_archive("erlang").unwrap().prefix()
        );
        assert_eq!(
            "https://github.com/forked/gleam",
            toolchain_manager.get_archive("gleam").unwrap().url()
        );
        assert_eq!(
            "sha1-test",
            toolchain_manager.get_archive("gleam").unwrap().sha1()
        );
    }
    */
}

*/
