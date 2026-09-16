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
mod workspace_id;
use finder::*;
pub use manager::*;
use paths::*;
pub use workspace_id::*;

use std::path::PathBuf;
use thiserror::*;

use crate::os::PathExt;

/// A workspace represents a particular project that you are working within.
///
/// It has a `name`, and a `root`. Both of these will be used to find artifacts that belong to this
/// workspace within the `ArtifactStore`.
///
#[derive(Builder, Default, Debug, Clone)]
#[builder(build_fn(error = "WorkspaceError"))]
pub struct Workspace {
    /// The name of the workspace.
    name: String,

    /// The root of the workspace. This is an absolute path and is host-dependant.
    #[builder(setter(custom))]
    root: PathBuf,
}

impl Workspace {
    pub fn builder() -> WorkspaceBuilder {
        WorkspaceBuilder::default()
    }

    pub fn name(&self) -> &str {
        self.name.as_ref()
    }

    pub fn root(&self) -> &PathBuf {
        &self.root
    }
}

impl WorkspaceBuilder {
    pub fn root(&mut self, root: impl Into<PathBuf>) -> &mut Self {
        self.root = Some(root.into().clean());
        self
    }
}

#[derive(Error, Debug)]
pub enum WorkspaceError {
    #[error("Attempted to build a Workspace struct while missing fields: {0:?}")]
    BuilderError(derive_builder::UninitializedFieldError),
}

impl From<derive_builder::UninitializedFieldError> for WorkspaceError {
    fn from(err: derive_builder::UninitializedFieldError) -> Self {
        Self::BuilderError(err)
    }
}
