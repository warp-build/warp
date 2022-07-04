use super::*;
use anyhow::*;
use std::{collections::HashMap, path::PathBuf};
use thiserror::*;
use url::Url;

pub const DEFAULT_IGNORE: [&str; 1] = ["warp-outputs"];

#[derive(Error, Debug)]
pub enum WorkspaceError {
    #[error("Attempted to build a Workspace while missing fields: {0:?}")]
    BuilderError(derive_builder::UninitializedFieldError),

    #[error(transparent)]
    WorkspaceScannerError(WorkspaceScannerError),

    #[error("{0}")]
    ValidationError(String),
}

impl From<derive_builder::UninitializedFieldError> for WorkspaceError {
    fn from(err: derive_builder::UninitializedFieldError) -> Self {
        Self::BuilderError(err)
    }
}

impl From<String> for WorkspaceError {
    fn from(s: String) -> Self {
        Self::ValidationError(s)
    }
}

/// A Workspace in Warp is a struct with all the information needed to interact
/// with your workspace.
///
/// This includes:
/// * all the paths to where things are
/// * descriptions of all the Targets in it
/// * descriptions of all the Archives used to build the Targets
///
#[derive(Clone, Debug, Builder)]
#[builder(build_fn(error = "anyhow::Error"))]
pub struct Workspace {
    /// The name of this workspace.
    pub name: String,

    pub workspace_file: WorkspaceFile,

    /// The current user.
    pub current_user: String,

    #[builder(default)]
    /// The URL to the remote cache service
    pub remote_cache_url: Option<Url>,

    /// The collection of paths required for a Warp Workspace to work.
    pub paths: WorkspacePaths,

    /// A map of aliases for commonly used targets
    pub aliases: HashMap<String, Label>,

    /// The archives defined in the workspace declaration file
    pub toolchain_configs: Vec<RuleConfig>,

    /// A list of local rules defined in this project
    pub local_rules: Vec<PathBuf>,

    /// A list of local toolchains defined in this project
    pub local_toolchains: Vec<PathBuf>,

    /// A list of patterns to be ignored from the repository
    pub ignore_patterns: Vec<String>,

    /// Whether to make sure we have git hooks installed or not
    pub use_git_hooks: bool,
}

impl Workspace {
    pub fn builder() -> WorkspaceBuilder {
        WorkspaceBuilder::default()
    }

    pub fn scanner(&self) -> WorkspaceScanner {
        WorkspaceScanner::new(&self.paths, &self.ignore_patterns)
    }
}

impl WorkspaceBuilder {
    pub async fn from_file(&mut self, file: WorkspaceFile) -> Result<&mut Self, anyhow::Error> {
        self.name(file.workspace.name.clone());

        let mut ignore_patterns = file.workspace.ignore_patterns.clone();
        let gitignore_patterns =
            git_ignore::read_patterns(&self.paths.as_ref().unwrap().workspace_root).await?;
        ignore_patterns.extend(gitignore_patterns);
        self.ignore_patterns(ignore_patterns);

        self.use_git_hooks(file.workspace.use_git_hooks);

        let mut aliases: HashMap<String, Label> = HashMap::default();
        for (name, label) in file.aliases.clone() {
            aliases.insert(name, label.into());
        }
        self.aliases(aliases);

        let mut toolchains: Vec<RuleConfig> = vec![];
        for toolchain in file.toolchains.clone() {
            toolchains.push(toolchain.try_into()?);
        }
        self.toolchain_configs(toolchains);

        self.workspace_file(file);

        Ok(self)
    }

    pub async fn find_rules_and_toolchains(&mut self) -> Result<&mut Self, anyhow::Error> {
        if self.paths.is_none() {
            return Err(anyhow::anyhow!("Attempted to scan while building a Workspace before setting the right paths. This is a bug."));
        }

        let (local_rules, local_toolchains) = {
            let scanner = WorkspaceScanner::new(&self.paths.as_ref().unwrap(), &vec![]);
            futures::join!(scanner.find_rules(), scanner.find_toolchains())
        };
        self.local_rules(local_rules.map_err(WorkspaceError::WorkspaceScannerError)?);
        self.local_toolchains(local_toolchains.map_err(WorkspaceError::WorkspaceScannerError)?);

        Ok(self)
    }
}
