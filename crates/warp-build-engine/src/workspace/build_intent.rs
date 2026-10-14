use super::{BuildClientConfig, Label};
use crate::{BuildProfile, CompilationTargetTriple, RustToolchainChannel};
use std::path::PathBuf;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BuildScope {
    All,
    Labels(Vec<Label>),
}

#[derive(Clone, Debug)]
pub struct BuildIntent {
    pub root: PathBuf,
    pub scope: BuildScope,
    pub output: Option<PathBuf>,
    pub cache: Option<PathBuf>,
    pub compilation_target: CompilationTargetTriple,
    pub rust_toolchain: Option<RustToolchainChannel>,
    pub profile: BuildProfile,
    pub verbose: bool,
    pub client: BuildClientConfig,
}
