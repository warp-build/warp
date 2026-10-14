use super::BuildClientConfig;
use crate::{BuildProfile, TargetTriple};
use std::path::PathBuf;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BuildScope {
    All,
    Scoped(Vec<PathBuf>),
}

#[derive(Clone, Debug)]
pub struct BuildIntent {
    pub root: PathBuf,
    pub scope: BuildScope,
    pub output: Option<PathBuf>,
    pub cache: Option<PathBuf>,
    pub target: TargetTriple,
    pub profile: BuildProfile,
    pub verbose: bool,
    pub client: BuildClientConfig,
}
