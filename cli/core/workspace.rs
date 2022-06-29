use super::*;
use std::fs;
use std::path::PathBuf;
use toml::{map::Map, Value};
use url::Url;

pub const WORKSPACE: &str = "Workspace.toml";

pub const DEFAULT_IGNORE: [&str; 1] = ["warp-outputs"];

/// A Workspace in Warp is a struct with all the information needed to interact
/// with your workspace.
///
/// This includes:
/// * all the paths to where things are
/// * descriptions of all the Targets in it
/// * descriptions of all the Archives used to build the Targets
///
#[derive(Clone, Debug)]
pub struct Workspace {
    /// The name of this workspace.
    pub name: String,

    /// The URL to the remote cache service
    pub remote_cache_url: Url,

    /// The collection of paths required for a Warp Workspace to work.
    pub paths: WorkspacePaths,

    pub aliases: WorkspaceAliases,

    /// The build files found in the workspace
    pub build_files: Vec<PathBuf>,

    /// The archives defined in the workspace declaration file
    pub toolchain_configs: Vec<RuleConfig>,

    /// A list of local rules defined in this project
    pub local_rules: Vec<PathBuf>,

    /// A list of local toolchains defined in this project
    pub local_toolchains: Vec<PathBuf>,

    pub ignore_patterns: Vec<String>,
}

impl Workspace {
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

    pub fn write_alias(self, alias: String, target: Label) -> Result<(), anyhow::Error> {
        let workspace_file = format!(
            "{}/{}",
            self.paths.workspace_root.to_str().unwrap(),
            "Workspace.toml"
        );
        let mut contents = fs::read_to_string(&workspace_file)?.parse::<Value>()?;
        let new_contents = contents.as_table_mut().unwrap();
        let mut alias_map = if let Some(previous_aliases) = new_contents.get("aliases") {
            previous_aliases.as_table().unwrap().clone()
        } else {
            Map::new()
        };

        alias_map.insert(alias, Value::String(target.to_string()));
        new_contents.insert("aliases".into(), Value::Table(alias_map));

        let new_toml = toml::to_string(new_contents)?;

        fs::write(workspace_file, new_toml).expect("Could not write TOML");

        return Ok(());
    }
}
