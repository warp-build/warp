use super::Label;
use std::path::PathBuf;

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Dependency {
    pub is_pinned: bool,
    pub label: Label,
    pub hash: String,
    pub outs: Vec<PathBuf>,
    pub srcs: Vec<PathBuf>,
}
