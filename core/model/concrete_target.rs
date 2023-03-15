use super::{Goal, Target, TargetId};
use crate::sync::*;
use std::path::{Path, PathBuf};

static CURRENT_DIR: &str = ".";

/// A ConcreteTarget is a target that has gone through the first phase of resolution.
///
#[derive(Builder, Clone, Debug, PartialEq, Eq)]
pub struct ConcreteTarget {
    goal: Goal,

    #[builder(setter(into))]
    target_id: TargetId,

    #[builder(setter(name = "target"))]
    original_target: Arc<Target>,

    #[builder(setter(into))]
    path: PathBuf,

    #[builder(setter(into))]
    workspace_root: PathBuf,

    #[builder(default, setter(into))]
    deps: Vec<TargetId>,
}

impl ConcreteTarget {
    pub fn builder() -> ConcreteTargetBuilder {
        Default::default()
    }

    pub fn new(
        goal: Goal,
        target_id: TargetId,
        original_target: Arc<Target>,
        path: PathBuf,
        workspace_root: PathBuf,
    ) -> Self {
        Self {
            deps: original_target.deps().to_vec(),
            original_target,
            target_id,
            path,
            goal,
            workspace_root,
        }
    }

    pub fn original_target(&self) -> Arc<Target> {
        self.original_target.clone()
    }

    pub fn dir(&self) -> &Path {
        if self.path.is_file() {
            let parent = self.path.parent().unwrap();
            if parent.to_string_lossy().is_empty() {
                Path::new(CURRENT_DIR)
            } else {
                parent
            }
        } else {
            &self.path
        }
    }

    pub fn path(&self) -> &Path {
        &self.path
    }

    pub fn name(&self) -> &str {
        self.path.file_name().unwrap().to_str().unwrap()
    }

    pub fn target_id(&self) -> TargetId {
        self.target_id
    }

    pub fn workspace_root(&self) -> &PathBuf {
        &self.workspace_root
    }
}

impl ToString for ConcreteTarget {
    fn to_string(&self) -> String {
        self.original_target.to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use assert_fs::prelude::*;

    #[test]
    fn dir_is_the_parent_of_the_full_path_for_file_targets() {
        let root = assert_fs::TempDir::new().unwrap();
        let target = root.child("target.ex");
        target.touch().unwrap();

        let ct = ConcreteTarget::new(
            Goal::Build,
            TargetId::next(),
            Arc::new(target.path().to_path_buf().into()),
            target.path().to_path_buf(),
            root.path().to_path_buf(),
        );
        assert_eq!(ct.dir(), root.path());
    }

    #[test]
    fn dir_is_never_empty_on() {
        let ct = ConcreteTarget::new(
            Goal::Build,
            TargetId::next(),
            Arc::new("./docs.ex".into()),
            "./docs.ex".into(),
            PathBuf::new(),
        );
        assert!(!ct.dir().to_string_lossy().is_empty());
    }
}
