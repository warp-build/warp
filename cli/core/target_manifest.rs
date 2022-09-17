use std::path::PathBuf;

#[derive(Debug, Clone)]
pub struct TargetManifest {
    pub outs: Vec<PathBuf>,
}
