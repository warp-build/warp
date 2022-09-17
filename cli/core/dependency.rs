use super::*;
use std::path::PathBuf;

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Dependency {
    pub rule_name: String,
    pub label: Label,
    pub hash: String,
    pub outs: Vec<PathBuf>,
    pub srcs: Vec<PathBuf>,
}
