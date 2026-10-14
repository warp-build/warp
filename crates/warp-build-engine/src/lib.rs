mod action_cache;
mod build;
mod error;
mod fingerprint;
pub mod graph;
mod request;
mod workspace;

pub use build::{build, BuildOutcome};
pub use graph::{
    ActionPlan, ExecutionReport, GraphError, GraphExecutor, Plan, TaskId, TaskProvider,
};
pub use request::{BuildRequest, Edition};
pub use workspace::{build_workspace, WorkspaceReport, WorkspaceRequest};

use crate::error::{Result, WarpError};
use std::path::Path;

pub fn crate_name(entry: &Path) -> Result<String> {
    let stem = entry
        .file_stem()
        .and_then(|stem| stem.to_str())
        .ok_or_else(|| WarpError::new("Rust entry point must have a UTF-8 file name"))?;
    let name = stem.replace('-', "_");
    let mut characters = name.chars();
    let valid_start = characters
        .next()
        .is_some_and(|character| character == '_' || character.is_ascii_alphabetic());
    if !valid_start
        || !characters.all(|character| character == '_' || character.is_ascii_alphanumeric())
    {
        return Err(WarpError::new(format!(
            "entry point {stem:?} does not produce a valid Rust crate name"
        )));
    }
    Ok(name)
}
