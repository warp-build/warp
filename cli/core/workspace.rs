use super::*;
use std::path::PathBuf;

pub const WORKSPACE: &str = "Workspace.toml";

pub const DEFAULT_IGNORE: [&str; 1] = ["zap-outputs"];

/// A Workspace in Zap is a struct with all the information needed to interact
/// with your workspace.
///
/// This includes:
/// * all the paths to where things are
/// * descriptions of all the Targets in it
/// * descriptions of all the Archives used to build the Targets
///
#[derive(Clone, Default, Debug)]
pub struct Workspace {
    /// The name of this workspace.
    pub name: String,

    /// The collection of paths required for a Zap Workspace to work.
    pub paths: WorkspacePaths,

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
}
