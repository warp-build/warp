//! # Managing, finding, and readying workspaces.
//!
//! This module takes care of finding, loading, saving, and validating Workspace configuration
//! files.
//!
//! It also takes care of readying any workspace related paths that need to be created and exist
//! before Warp can begin executing, as they are part of what makes a Workspace valid.
//!
//! > NOTE(@ostera): in the long run, we probably want the `WorkspacePaths` to be part of the
//! `Config` to make sure we can be a bit more flexible, especially when running Warp Cloud.
//!

mod finder;
mod manager;
mod paths;
mod workspace;
mod workspace_id;
use finder::*;
pub use manager::*;
use paths::*;
pub use workspace::*;
use workspace_id::*;
