pub mod action_cache;
mod build;
pub mod error;
mod fingerprint;
pub mod graph;
mod progress;
pub mod request;
mod workspace;

pub use build::{build, BuildOutcome};
pub use fingerprint::host_triple;
pub use graph::{
    ActionPlan, ExecutionReport, GraphError, GraphExecutor, Plan, TaskId, TaskProvider,
};
pub use progress::{status_tag, stdout_status_tag};
pub use request::{
    default_cache_directory, BuildProfile, BuildRequest, CompilationTargetTriple, Edition,
    RustToolchainChannel,
};

pub fn rustc_info() -> Result<(std::path::PathBuf, String)> {
    fingerprint::rustc()
}
pub use error::{Result, WarpError};
pub use workspace::{
    build_workspace, BuildClientConfig, BuildCtx, BuildIntent, BuildScope, BuildTarget, Label,
    WorkspaceReport,
};

use std::path::Path;

pub fn rustc_output_filename(
    compilation_target: &CompilationTargetTriple,
    crate_name: &str,
    crate_type: &str,
) -> Result<String> {
    let (compiler, _) = fingerprint::rustc()?;
    request::rustc_output_filename(&compiler, compilation_target, crate_name, crate_type)
}

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
