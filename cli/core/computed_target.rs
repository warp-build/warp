use super::Event;
use super::*;
use fxhash::*;
use seahash::SeaHasher;
use std::fs::File;
use std::hash::{Hash, Hasher};
use std::io::{BufReader, Read};
use std::path::PathBuf;
use std::sync::Arc;
use thiserror::Error;
use tracing::*;

#[derive(Error, Debug)]
pub enum ComputedTargetError {
    #[error("The target name `{label:?}` is missing dependencies: {deps:?}")]
    MissingDependencies { label: Label, deps: Vec<Label> },

    #[error("Something went wrong.")]
    Unknown,
}

#[derive(Debug, Clone)]
pub struct ComputedTarget {
    /// The original Target that this Computed Target was derived from.
    pub target: Target,

    /// The hash of this node
    pub hash: Option<String>,

    /// The dependencies of this target.
    pub deps: Option<FxHashSet<Dependency>>,

    /// The transitive dependencies of this target.
    pub transitive_deps: Option<FxHashSet<Dependency>>,

    /// The outputs of this node
    pub outs: Option<FxHashSet<PathBuf>>,

    pub run_script: Option<RunScript>,

    /// The inputs of this node
    pub srcs: Option<FxHashSet<PathBuf>>,

    /// The actions to reify this target
    pub actions: Option<Vec<Action>>,

    /// Whether this ComputedTarget has already been successfully built, is waiting
    /// to be built, or has been found in the cache
    pub status: ComputeStatus,
}

#[derive(Debug, Clone)]
pub enum ComputeStatus {
    Uninitialized,
    Pending,
    CacheHit,
    Succeeded,
    Failed,
}

impl Default for ComputeStatus {
    fn default() -> ComputeStatus {
        ComputeStatus::Pending
    }
}

impl ComputedTarget {
    pub fn from_global_target(target: Target) -> ComputedTarget {
        ComputedTarget {
            target,
            status: ComputeStatus::Pending,
            actions: None,
            deps: Some(FxHashSet::default()),
            transitive_deps: None,
            hash: None,
            outs: Some(FxHashSet::default()),
            srcs: Some(FxHashSet::default()),
            run_script: None,
        }
    }

    pub fn from_target(target: Target) -> ComputedTarget {
        ComputedTarget {
            target,
            status: ComputeStatus::Pending,
            actions: None,
            deps: None,
            transitive_deps: None,
            hash: None,
            outs: None,
            srcs: None,
            run_script: None,
        }
    }

    pub fn from_target_with_deps(
        target: Target,
        find_node: &dyn Fn(Label) -> Option<ComputedTarget>,
    ) -> Result<ComputedTarget, ComputedTargetError> {
        let mut deps: FxHashSet<Dependency> = FxHashSet::default();
        let mut missing_deps: FxHashSet<Label> = FxHashSet::default();

        for dep in target.deps() {
            if let Some(node) = find_node(dep.clone()) {
                deps.insert(node.as_dep());
            } else {
                missing_deps.insert(dep.clone());
            }
        }

        if !missing_deps.is_empty() {
            return Err(ComputedTargetError::MissingDependencies {
                label: target.label().clone(),
                deps: missing_deps.iter().cloned().collect(),
            });
        }

        Ok(ComputedTarget {
            target,
            status: ComputeStatus::Pending,
            actions: None,
            deps: Some(deps),
            transitive_deps: None,
            hash: None,
            outs: None,
            srcs: None,
            run_script: None,
        })
    }

    pub fn as_dep(&self) -> Dependency {
        Dependency {
            label: self.target.label().clone(),
            hash: self.hash(),
            outs: self.outs(),
            srcs: self.srcs(),
        }
    }

    pub fn mark_failed(&mut self) {
        self.status = ComputeStatus::Failed;
    }

    pub fn mark_succeeded(&mut self) {
        self.status = ComputeStatus::Succeeded;
    }

    pub fn hash(&self) -> String {
        self.hash.clone().unwrap_or_else(|| {
            panic!(
                "ComputedTarget {:?} has not been hashed yet!",
                self.label().to_string()
            )
        })
    }

    pub fn label(&self) -> &Label {
        self.target.label()
    }

    pub fn srcs(&self) -> Vec<PathBuf> {
        let mut srcs: Vec<PathBuf> = self
            .srcs
            .as_ref()
            .unwrap_or_else(|| {
                panic!(
                    "ComputedTarget {:?} does not have computed inputs yet!",
                    self.label().to_string()
                )
            })
            .iter()
            .cloned()
            .collect();
        srcs.sort();
        srcs
    }

    pub fn outs(&self) -> Vec<PathBuf> {
        let mut outs: Vec<PathBuf> = self
            .outs
            .as_ref()
            .unwrap_or_else(|| {
                panic!(
                    "ComputedTarget {:?} does not have computed outputs yet!",
                    self.label().to_string()
                )
            })
            .iter()
            .cloned()
            .collect();
        outs.sort();
        outs
    }

    pub fn deps(&self) -> Vec<Dependency> {
        let mut deps: Vec<Dependency> = self
            .deps
            .as_ref()
            .unwrap_or_else(|| {
                panic!(
                    "ComputedTarget {:?} does not have computed dependencies yet!",
                    self.label().to_string()
                )
            })
            .iter()
            .cloned()
            .collect();
        deps.sort();
        deps
    }

    #[tracing::instrument(name="ComputedTarget::transitive_deps", skip(self, find_node), fields(warp.target = %self.label().to_string()))]
    pub fn transitive_deps(
        &mut self,
        find_node: &dyn Fn(Label) -> Option<ComputedTarget>,
    ) -> Result<Vec<Dependency>, ComputedTargetError> {
        if let Some(deps) = &self.transitive_deps {
            return Ok(deps.iter().cloned().collect());
        }

        let mut deps: FxHashSet<Dependency> = FxHashSet::default();
        let mut missing_deps: FxHashSet<Label> = FxHashSet::default();

        if let Some(this_deps) = &self.deps {
            for dep in this_deps {
                if let Some(mut node) = find_node(dep.label.clone()) {
                    deps.insert(dep.clone());
                    for dep in node.transitive_deps(find_node)? {
                        deps.insert(dep);
                    }
                } else {
                    missing_deps.insert(dep.label.clone());
                }
            }
        }

        if !missing_deps.is_empty() {
            let mut missing_deps = missing_deps.iter().cloned().collect::<Vec<Label>>();
            missing_deps.sort();
            Err(ComputedTargetError::MissingDependencies {
                label: self.target.label().clone(),
                deps: missing_deps,
            })
        } else {
            let transitive_deps = deps;
            let mut deps = transitive_deps.iter().cloned().collect::<Vec<Dependency>>();
            deps.sort();
            self.transitive_deps = Some(transitive_deps);
            Ok(deps.to_vec())
        }
    }

    pub fn actions(&self) -> Vec<Action> {
        self.actions.clone().unwrap_or_else(|| {
            panic!(
                "ComputedTarget {:?} does not have computed actions yet!",
                self.label().to_string()
            )
        })
    }

    #[tracing::instrument(name = "ComputedTarget::execute", skip(self))]
    pub async fn execute(
        &self,
        cache_root: &PathBuf,
        sandbox_root: &PathBuf,
        mode: ExecutionMode,
        event_channel: Arc<EventChannel>,
    ) -> Result<(), anyhow::Error> {
        trace!(
            "Executing {:?} target {}...",
            self.target.kind(),
            self.target.label().to_string(),
        );

        let node_cache_path = cache_root.join(&self.hash());

        trace!("Running actions for {}...", self.target.label().to_string());
        event_channel.send(Event::PreparingActions {
            label: self.target.label().clone(),
            action_count: self.actions().len().try_into().unwrap(),
        });
        for action in self.actions() {
            event_channel.send(Event::ActionRunning {
                label: self.target.label().clone(),
                action: action.clone(),
            });
            action
                .run(
                    self.target.label().clone(),
                    &node_cache_path,
                    sandbox_root,
                    event_channel.clone(),
                )
                .await?
        }

        Ok(())
    }

    /// The hash of a build node serves for caching work:
    /// * the hash of the dependencies
    /// * listed inputs, and their contents
    /// * listed outputs
    /// * rule name
    /// * the hash of the computed actions that this target will execute
    ///
    pub fn recompute_hash(&mut self) {
        let mut s = SeaHasher::new();

        self.target.label().hash(&mut s);

        /*
        guess_host_triple::guess_host_triple()
            .unwrap()
            .to_string()
            .hash(&mut s);
        */

        let deps = self.deps.as_ref().unwrap().iter().map(|d| d.hash.as_str());

        let actions: Vec<String> = self
            .actions
            .as_ref()
            .unwrap()
            .iter()
            .map(|a| format!("{:?}", a))
            .collect();

        let mut srcs: Vec<&PathBuf> = self.srcs.as_ref().unwrap().iter().collect();
        srcs.dedup_by(|a, b| a == b);
        srcs.sort();

        let mut seeds: Vec<&str> = {
            deps.chain(
                self.outs
                    .as_ref()
                    .unwrap()
                    .iter()
                    .map(|o| o.to_str().unwrap()),
            )
            .chain(actions.iter().map(|a| a.as_str()))
            .chain(srcs.iter().map(|s| s.to_str().unwrap()))
            .collect()
        };

        seeds.dedup_by(|a, b| a == b);
        seeds.sort_unstable();

        for seed in seeds {
            seed.hash(&mut s);
        }

        for src in srcs {
            let f = File::open(&src).expect(&format!("Unable to open: {:?}", &src));
            let mut buffer = [0; 2048];
            let mut reader = BufReader::new(f);
            while let Ok(len) = reader.read(&mut buffer) {
                if len == 0 {
                    break;
                }
                buffer[..len].hash(&mut s);
            }
        }

        let hash = s.finish();
        self.hash = Some(hash.to_string());
    }
}

impl Default for ComputedTarget {
    fn default() -> Self {
        panic!("Oops! We have attempted to create a default target. This means the DepGraph was somehow incomplete. This is a bug!");
    }
}

/*
#[cfg(test)]
mod tests {
    use super::*;
    use crate::Rule;
    use std::path::PathBuf;

    fn toolchain_mgr() -> ToolchainManager {
        ToolchainManager::new()
    }

    fn unsealed_target() -> ComputedTarget {
        ComputedTarget::from_target(Box::new(TestTarget::new()))
    }

    fn sealed_target() -> ComputedTarget {
        let mut target = ComputedTarget::from_target(Box::new(TestTarget::new()));
        target.seal(&vec![], &toolchain_mgr()).unwrap();
        target
    }

    #[test]
    #[should_panic]
    fn must_be_sealed_to_access_srcs() {
        let target = unsealed_target();
        target.srcs();
    }

    #[test]
    #[should_panic]
    fn must_be_sealed_to_access_outs() {
        let target = unsealed_target();
        target.outs();
    }

    #[test]
    #[should_panic]
    fn must_be_sealed_to_access_deps() {
        let target = unsealed_target();
        target.deps();
    }

    #[test]
    #[should_panic]
    fn must_be_sealed_to_access_hash() {
        let target = unsealed_target();
        target.hash();
    }

    #[test]
    fn can_access_srcs_after_sealing() {
        let target = sealed_target();
        target.srcs();
    }

    #[test]
    fn can_access_outs_after_sealing() {
        let target = sealed_target();
        target.outs();
    }

    #[test]
    fn can_access_deps_after_sealing() {
        let target = sealed_target();
        target.deps();
    }

    #[test]
    fn can_access_hash_after_sealing() {
        let target = sealed_target();
        target.hash();
    }

    #[test]
    fn srcs_are_part_of_hashing() {
        let a = sealed_target();

        let mut b = TestTarget::new();
        b.set_srcs(&vec![PathBuf::from("src")]);
        let mut b = ComputedTarget::from_target(Box::new(b));
        b.seal(&vec![], &toolchain_mgr()).unwrap();

        assert_ne!(a.hash, b.hash);
    }

    #[test]
    fn deps_are_part_of_hashing() {
        let a = sealed_target();

        let mut b = ComputedTarget::from_target(Box::new(TestTarget::new()));
        b.seal(
            &vec![Dependency {
                label: Label::new(":a"),
                hash: "some-hash".to_string(),
                outs: vec![],
            }],
            &toolchain_mgr(),
        )
        .unwrap();

        assert_ne!(a.hash, b.hash);
    }
}
*/
