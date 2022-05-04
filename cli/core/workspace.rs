use super::*;
use std::path::PathBuf;

pub const WORKSPACE: &str = "Workspace.toml";

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
    pub toolchain_archives: Vec<Archive>,

    /// A list of local rules defined in this project
    pub local_rules: Vec<PathBuf>,

    /// A list of local toolchains defined in this project
    pub local_toolchains: Vec<PathBuf>,
}