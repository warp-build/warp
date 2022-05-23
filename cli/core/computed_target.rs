use super::*;
use anyhow::{anyhow, Context};
use fxhash::*;
use log::*;
use seahash::SeaHasher;
use std::fs;
use std::hash::{Hash, Hasher};
use std::path::PathBuf;
use std::process::Command;
use std::process::Stdio;
use thiserror::Error;

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

    /// The outputs of this node
    pub outs: Option<FxHashSet<PathBuf>>,

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
            hash: None,
            outs: Some(FxHashSet::default()),
            srcs: Some(FxHashSet::default()),
        }
    }

    pub fn from_target(target: Target) -> ComputedTarget {
        ComputedTarget {
            target,
            status: ComputeStatus::Pending,
            actions: None,
            deps: None,
            hash: None,
            outs: None,
            srcs: None,
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

        if missing_deps.len() > 0 {
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
            hash: None,
            outs: None,
            srcs: None,
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
        if self.target.is_local() {
            self.hash.clone().unwrap_or_else(|| {
                panic!(
                    "ComputedTarget {:?} has not been hashed yet!",
                    self.label().to_string()
                )
            })
        } else {
            self.target.archive().unwrap().hash()
        }
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

    pub fn transitive_deps(
        &self,
        find_node: &dyn Fn(Label) -> Option<ComputedTarget>,
    ) -> Result<Vec<Dependency>, ComputedTargetError> {
        trace!(
            "Getting deps for {}: {:?}",
            &self.label().to_string(),
            &self.deps
        );

        let mut deps: FxHashSet<Dependency> = FxHashSet::default();
        let mut missing_deps: FxHashSet<Label> = FxHashSet::default();

        if let Some(this_deps) = &self.deps {
            for dep in this_deps {
                if let Some(node) = find_node(dep.label.clone()) {
                    trace!(
                        "Getting transitive deps for {}: {:?}",
                        &node.label().to_string(),
                        &node.deps
                    );
                    deps.insert(dep.clone());
                    for dep in node.transitive_deps(find_node)? {
                        deps.insert(dep);
                    }
                } else {
                    missing_deps.insert(dep.label.clone());
                }
            }
        }

        let mut deps = deps.iter().cloned().collect::<Vec<Dependency>>();
        deps.sort();
        let mut missing_deps = missing_deps.iter().cloned().collect::<Vec<Label>>();
        missing_deps.sort();

        if missing_deps.len() > 0 {
            Err(ComputedTargetError::MissingDependencies {
                label: self.target.label().clone(),
                deps: missing_deps,
            })
        } else {
            Ok(deps)
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

    pub fn execute(
        &self,
        archive_root: &PathBuf,
        cache_root: &PathBuf,
        sandbox_root: &PathBuf,
        mode: ExecutionMode,
    ) -> Result<(), anyhow::Error> {
        trace!(
            "Executing {:?} target {}...",
            self.target.kind(),
            self.target.label().to_string(),
        );
        if let Some(archive) = &self.target.archive() {
            trace!("Target has an archive, preparing...");

            // TODO(@ostera): move this _into_ the Archive
            let archive_root = archive_root.join(format!("{}-{}", archive.name(), archive.hash()));

            if !archive.is_cached(&archive_root)? {
                archive.download(&archive_root)?;
                match archive.checksum(&archive_root) {
                    Ok(_) => archive.unpack(&archive_root, &cache_root),
                    Err(e) => {
                        archive.clean(&archive_root)?;
                        Err(e)
                    }
                }?
            }
        }

        trace!("Running actions for {}...", self.target.label().to_string());
        for action in self.actions() {
            action.run(&sandbox_root)?
        }

        if self.target.kind() == TargetKind::Runnable && mode == ExecutionMode::BuildAndRun {
            print!("🔨 Running {}...", self.target.label().to_string());

            match &self.outs {
                Some(outs) if outs.len() > 0 => {
                    let outs: Vec<PathBuf> = outs.iter().cloned().collect();
                    let path = sandbox_root.join(PathBuf::from(outs[0].clone()));
                    let mut cmd = Command::new(path);

                    cmd.current_dir(sandbox_root);
                    cmd.stdin(Stdio::inherit())
                        .stderr(Stdio::inherit())
                        .stdout(Stdio::inherit());

                    trace!("Spawning {:?}", &cmd);
                    let mut proc = cmd.spawn()?;

                    trace!("Waiting on {:?}", &cmd);
                    proc.wait().map(|_| ()).context(format!(
                        "Error executing {}",
                        &self.target.label().to_string()
                    ))?;

                    trace!("Exited with status: {}", cmd.status()?);
                    Ok(())
                }
                _ => Err(anyhow!(
                    "Target {} has no outputs!",
                    &self.target.label().to_string()
                )),
            }
        } else {
            Ok(())
        }
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

        let deps = self
            .deps
            .as_ref()
            .unwrap()
            .into_iter()
            .map(|d| d.hash.as_str());

        let actions: Vec<String> = self
            .actions
            .as_ref()
            .unwrap()
            .iter()
            .map(|a| format!("{:?}", a))
            .collect();

        let mut srcs: Vec<&PathBuf> = self.srcs.as_ref().unwrap().iter().collect();
        srcs.dedup_by(|a, b| a == b);
        let srcs: Vec<String> = srcs.iter()
            .map(|src| fs::read_to_string(&src).unwrap_or_else(|_| panic!("Truly expected {:?} to be a readable file. Was it changed since the build started?", src)))
            .collect();

        let mut seeds: Vec<&str> = {
            deps.chain(
                self.outs
                    .as_ref()
                    .unwrap()
                    .iter()
                    .map(|o| o.to_str().unwrap()),
            )
            .chain(actions.iter().map(|a| a.as_str()))
            .chain(srcs.iter().map(|s| s.as_str()))
            .collect()
        };

        seeds.dedup_by(|a, b| a == b);
        seeds.sort();

        for seed in seeds {
            seed.hash(&mut s);
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
