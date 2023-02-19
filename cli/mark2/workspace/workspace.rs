use std::path::PathBuf;
use thiserror::*;

/// A workspace represents a particular project that you are working within.
///
/// It has a `name`, and a `root`. Both of these will be used to find artifacts that belong to this
/// workspace within the `ArtifactStore`.
///
#[derive(Builder, Debug, Clone)]
#[builder(build_fn(error = "WorkspaceError"))]
pub struct Workspace {
    /// The name of the workspace.
    name: String,

    /// The root of the workspace. This is an absolute path and is host-dependant.
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
