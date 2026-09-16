use super::{Goal, Target, TargetId};
use crate::sync::*;
use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};
use tracing::instrument;

static CURRENT_DIR: &str = ".";

/// A ConcreteTarget is a target that has gone through the first phase of resolution.
///
#[derive(Builder, Clone, Debug, Ord, PartialOrd, PartialEq, Eq, Serialize, Deserialize)]
pub struct ConcreteTarget {
    goal: Goal,

    #[serde(skip)]
    #[builder(setter(into))]
    target_id: TargetId,

    #[builder(setter(name = "target"))]
    original_target: Arc<Target>,

    #[builder(setter(into))]
    path: PathBuf,

    #[builder(setter(into))]
    workspace_root: PathBuf,
}

impl std::hash::Hash for ConcreteTarget {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.goal.hash(state);
        self.original_target.hash(state);
        self.path.hash(state);
        self.workspace_root.hash(state);
    }
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
            original_target,
            target_id,
            path,
            goal,
            workspace_root,
        }
    }

    pub fn set_target(&mut self, target: Arc<Target>, target_id: TargetId) {
        self.original_target = target;
        self.target_id = target_id;
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

    #[instrument(name = "ConcreteTarget::name", skip(self), ret)]
    pub fn name(&self) -> &str {
        // NB(@ostera): sometimes [self.path] does not have a file name, since its a single
        // components. This happens when the ConcreteTarget is coming from a RemoteTarget and we
        // have an interesting subpath on it.
        if let Some(file) = self.path.file_name() {
            file.to_str().unwrap()
        } else {
            self.path.to_str().unwrap()
        }
    }

    pub fn target_id(&self) -> TargetId {
        self.target_id
    }

    pub fn workspace_root(&self) -> &PathBuf {
        &self.workspace_root
    }

    pub fn goal(&self) -> Goal {
        self.goal
    }

    pub fn abs_path(&self) -> PathBuf {
        self.workspace_root.join(&self.path)
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
