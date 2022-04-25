use super::Label;
use std::path::PathBuf;

#[derive(Clone, Debug)]
pub struct Dependency {
    pub label: Label,
    pub hash: String,
    pub outs: Vec<PathBuf>,
}
