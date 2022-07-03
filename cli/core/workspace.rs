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
    pub fn from_file(&mut self, file: WorkspaceFile) -> Result<&mut Self, anyhow::Error> {
        self.name(file.workspace.name.clone());

        self.ignore_patterns(file.workspace.ignore_patterns.clone());

        self.use_git_hooks(file.workspace.use_git_hooks);

        self.remote_cache_url(file.workspace.remote_cache_url.clone());

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

/*
pub fn with_current_user(&mut self, current_user: String) -> &mut Workspace {
    self.current_user = current_user;
    self
}

pub fn with_rules(self, rules: &[PathBuf]) -> Workspace {
    Workspace {
        local_rules: rules.to_vec(),
        ..self
    }
}

pub fn with_toolchains(self, toolchains: &[PathBuf]) -> Workspace {
    Workspace {
        local_toolchains: toolchains.to_vec(),
        ..self
    }
}

pub fn with_gitignore_patterns(self, gitignore_patterns: Vec<String>) -> Workspace {
    let mut new_ignore_patterns = self.ignore_patterns.clone();
    new_ignore_patterns.extend(gitignore_patterns);

    Workspace {
        ignore_patterns: new_ignore_patterns,
        ..self
    }
}

pub async fn write_alias(self, alias: String, target: Label) -> Result<(), anyhow::Error> {
    let workspace_file = self.paths.workspace_root.join(WORKSPACE);

    let mut contents = fs::read_to_string(&workspace_file)
        .await?
        .parse::<Value>()?;

    let new_contents = contents
        .as_table_mut()
        .expect("We expect workspace to be a table");

    let mut alias_map = if let Some(previous_aliases) = new_contents.get("aliases") {
        previous_aliases.as_table().unwrap().clone()
    } else {
        Map::new()
    };

    alias_map.insert(alias, Value::String(target.to_string()));
    new_contents.insert("aliases".into(), Value::Table(alias_map));

    let new_toml = toml::to_string(new_contents)?;

    fs::write(workspace_file, new_toml)
        .await
        .expect("Could not write TOML");

    return Ok(());
}
*/
