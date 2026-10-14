use std::path::PathBuf;

#[derive(Clone, Debug)]
pub(super) enum BuildNode {
    ScanDirectory(PathBuf),
    PackagePath(PathBuf),
}
