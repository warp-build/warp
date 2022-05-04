use super::*;
use anyhow::{anyhow, Context};
use std::collections::HashSet;
use crypto::digest::Digest;
use crypto::sha1::Sha1;
use log::*;
use std::fs;
use std::path::PathBuf;
use std::process::Command;
use std::process::Stdio;

#[derive(Debug, Clone)]
pub struct ComputedTarget {
    /// The original Target that this Computed Target was derived from.
    pub target: Target,

    /// The hash of this node
    pub hash: Option<String>,

    /// The dependencies of this target.
    pub deps: Option<Vec<Dependency>>,

    /// The outputs of this node
    pub outs: Option<Vec<PathBuf>>,

    /// The inputs of this node
    pub srcs: Option<Vec<PathBuf>>,

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
        self.srcs.clone().unwrap_or_else(|| {
            panic!(
                "ComputedTarget {:?} does not have computed inputs yet!",
                self.label().to_string()
            )
        })
    }

    pub fn outs(&self) -> Vec<PathBuf> {
        self.outs.clone().unwrap_or_else(|| {
            panic!(
                "ComputedTarget {:?} does not have computed outputs yet!",
                self.label().to_string()
            )
        })
    }

    pub fn deps(&self) -> Vec<Dependency> {
        self.deps.clone().unwrap_or_else(|| {
            panic!(
                "ComputedTarget {:?} does not have computed dependencies yet!",
                self.label().to_string()
            )
        })
    }

    pub fn transitive_deps(&self, dep_graph: &DepGraph) -> Vec<Dependency> {
        trace!("Getting deps for {}: {:?}", &self.label().to_string(), &self.deps);

        let mut deps = vec![];

        if let Some(this_deps) = &self.deps {
            for dep in this_deps {
                let node = dep_graph.find_node(&dep.label).unwrap();
                trace!("Getting transitive deps for {}: {:?}", &node.label().to_string(), &node.deps);
                deps.push(dep.clone());
                let mut dep_deps = node.transitive_deps(&dep_graph);
                deps.append(&mut dep_deps);
            }
        }

        deps
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

        for action in self.actions() {
            action.run()?
        }

        if self.target.kind() == TargetKind::Runnable {
            print!("🔨 Running {}...", self.target.label().to_string());

            match &self.outs {
                Some(outs) if outs.len() > 0 => {
                    let mut cmd = Command::new(PathBuf::from(outs[0].clone()));

                    cmd.stdin(Stdio::inherit())
                        .stderr(Stdio::inherit())
                        .stdout(Stdio::inherit());

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
        let mut hasher = Sha1::new();

        let name = self.target.label();
        hasher.input_str(&name.to_string());

        let mut seeds: HashSet<String> = HashSet::new();

        for d in self.deps.as_ref().unwrap() {
            seeds.insert(d.hash.clone());
        }

        for src_path in self.srcs.as_ref().unwrap() {
            let contents = fs::read_to_string(&src_path)
                .unwrap_or_else(|_| panic!("Truly expected {:?} to be a readable file. Was it changed since the build started?", src_path));
            let mut hasher = Sha1::new();
            hasher.input_str(&contents);
            seeds.insert(hasher.result_str());
        }

        for o in self.outs.as_ref().unwrap() {
            seeds.insert(o.to_str().unwrap().to_string());
        }

        for a in self.actions.as_ref().unwrap() {
            // TODO(@ostera): implement Hash for Action
            seeds.insert(format!("{:?}", a));
        }

        let mut sorted_seeds: Vec<String> = seeds.iter().cloned().collect();
        sorted_seeds.sort();

        for seed in sorted_seeds {
            hasher.input_str(&seed);
        }

        let hash = hasher.result_str();
        self.hash = Some(hash);
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