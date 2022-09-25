use super::*;
use anyhow::*;
use fxhash::*;
use std::{
    convert::{TryFrom, TryInto},
    path::PathBuf,
};
use thiserror::*;
use url::Url;

pub const DEFAULT_IGNORE: [&str; 2] = ["warp-outputs", ".git"];

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

    /// The current user.
    pub current_user: String,

    #[builder(default)]
    /// The URL to the remote cache service
    pub remote_cache_url: Option<Url>,

    #[builder(default)]
    /// The collection of paths required for a Warp Workspace to work.
    pub paths: WorkspacePaths,

    #[builder(default)]
    /// A map of aliases for commonly used targets
    pub aliases: FxHashMap<String, Label>,

    #[builder(default)]
    /// The archives defined in the workspace declaration file
    pub toolchain_configs: FxHashMap<String, RuleConfig>,

    #[builder(default)]
    /// The remote workspaces
    pub remote_workspaces: FxHashMap<String, RemoteWorkspace>,

    #[builder(default)]
    /// A list of rules that have been downloaded
    pub global_rules: Vec<PathBuf>,

    #[builder(default)]
    /// A list of local rules defined in this project
    pub local_rules: Vec<PathBuf>,

    #[builder(default)]
    /// A list of local toolchains defined in this project
    pub local_toolchains: Vec<PathBuf>,

    #[builder(default)]
    /// A list of patterns to be ignored from the repository
    pub ignore_patterns: Vec<String>,

    #[builder(default)]
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

impl Default for Workspace {
    fn default() -> Self {
        let mut builder = Self::builder();
        builder
            .name("default-workspace".to_string())
            .current_user("user".to_string())
            .build()
            .unwrap()
    }
}

impl WorkspaceBuilder {
    pub async fn from_file(&mut self, file: WorkspaceFile) -> Result<&mut Self, anyhow::Error> {
        self.name(file.workspace.name.clone());

        let mut ignore_patterns = vec![];
        for pat in DEFAULT_IGNORE {
            ignore_patterns.push(pat.to_string());
        }
        ignore_patterns.extend(file.workspace.ignore_patterns.clone());
        self.ignore_patterns(ignore_patterns);

        self.use_git_hooks(file.workspace.use_git_hooks);

        if let Some(url) = &file.workspace.remote_cache_url {
            let url: Url = url.parse().expect("remote_cache_url should be a valid URL");
            self.remote_cache_url(Some(url));
        }

        let mut aliases: FxHashMap<String, Label> = FxHashMap::default();
        for (name, label) in file.aliases.clone() {
            aliases.insert(format!("@{}", name), label.into());
        }
        self.aliases(aliases);

        let mut toolchains: FxHashMap<String, RuleConfig> = FxHashMap::default();
        for (name, config) in file.toolchains {
            toolchains.insert(name.clone(), config.try_into()?);
        }
        self.toolchain_configs(toolchains);

        let mut remote_workspaces: FxHashMap<String, RemoteWorkspace> = FxHashMap::default();
        for (name, config) in file.remote_workspaces {
            remote_workspaces.insert(name.clone(), config.try_into()?);
        }
        self.remote_workspaces(remote_workspaces);

        Ok(self)
    }

    pub async fn find_rules_and_toolchains(&mut self) -> Result<&mut Self, anyhow::Error> {
        if self.paths.is_none() {
            return Err(anyhow::anyhow!("Attempted to scan while building a Workspace before setting the right paths. This is a bug."));
        }

        let (local_rules, local_toolchains, global_rules) = {
            let scanner = WorkspaceScanner::new(self.paths.as_ref().unwrap(), &[]);
            futures::join!(
                scanner.find_rules(),
                scanner.find_toolchains(),
                scanner.find_global_rules()
            )
        };
        self.global_rules(global_rules.map_err(WorkspaceError::WorkspaceScannerError)?);
        self.local_rules(local_rules.map_err(WorkspaceError::WorkspaceScannerError)?);
        self.local_toolchains(local_toolchains.map_err(WorkspaceError::WorkspaceScannerError)?);

        Ok(self)
    }
}
