use super::Label;
use std::path::PathBuf;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Dependency {
    pub label: Label,
    pub hash: String,
    pub outs: Vec<PathBuf>,
    pub srcs: Vec<PathBuf>,
}
