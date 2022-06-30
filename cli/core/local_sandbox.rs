use super::*;
use anyhow::{anyhow, Context};
use futures::FutureExt;
use fxhash::*;
use std::path::PathBuf;
use std::sync::Arc;
use tokio::fs;
use tracing::*;

mod error {
    use crate::ComputedTargetError;
    use crate::Label;
    use std::path::PathBuf;
    use thiserror::Error;

    #[derive(Error, Debug)]
    pub enum SandboxError {
        #[error(transparent)]
        Unknown(anyhow::Error),

        #[error(transparent)]
        MissingDependencies(ComputedTargetError),

        #[error("Could not create directory {dst:?} at: {dst_parent:?}")]
        CouldNotCreateDir { dst: PathBuf, dst_parent: PathBuf },

        #[error("When building {label:?}, could not link {src:?} into sandbox at {dst:?}")]
        CouldNotLink {
            label: Label,
            src: PathBuf,
            dst: PathBuf,
        },

        #[error(r"When building {}, we failed to execute some actions.

Find the sandbox in: {sandbox_root:?}

{error:?}", .label.to_string())]
        ExecutionError {
            label: Label,
            sandbox_root: PathBuf,
            error: anyhow::Error,
        },
    }
}

/// A build Sandbox.
///
/// This is a spot where we isolate build nodes to execute them.
///
/// By the time we have created a Sandbox for a particular ComputedTarget, the node
/// is already _known_ to need building. This means that the node will already
/// be hashed.
///
/// The sandboxing steps are as folllows:
///
///   1. ensure the rule outputs are safe
///   2. prepare sandbox dir
///   3. copy dependences & inputs into sandbox
///   4. enter sandbox
///      5. build the node rule
///      6. validate rule's outputs
///   7. exit sandbox
///
///
pub struct LocalSandbox {
    /// The node to be built.
    node: ComputedTarget,

    /// The path to this sandbox on disk
    root: PathBuf,
    outputs_root: PathBuf,

    /// The outputs created during this build
    outputs: Vec<PathBuf>,

    status: ValidationStatus,

    workspace: Workspace,
}

#[derive(Debug, Clone)]
pub enum ValidationStatus {
    Pending,
    Invalid {
        expected_and_present: Vec<PathBuf>,
        expected_but_missing: Vec<PathBuf>,
        unexpected_but_present: Vec<PathBuf>,
    },
    NoOutputs,
    Valid,
}

impl LocalSandbox {
    #[tracing::instrument(name="LocalSandbox::for_node", skip(workspace, node), fields(warp.target = %node.target.label().to_string()))]
    pub fn for_node(workspace: &Workspace, node: ComputedTarget) -> LocalSandbox {
        let workspace = workspace.clone();
        let root = workspace.paths.local_sandbox_root.join(node.hash());
        let outputs_root = workspace.paths.local_outputs_root.clone();
        LocalSandbox {
            node,
            outputs: vec![],
            root,
            outputs_root,
            status: ValidationStatus::Pending,
            workspace,
        }
    }

    pub fn node(&self) -> &ComputedTarget {
        &self.node
    }

    pub fn root(&self) -> PathBuf {
        self.root.clone()
    }

    pub fn outputs(&self) -> Vec<PathBuf> {
        self.outputs.clone()
    }

    fn scan_files(root: &PathBuf) -> futures::future::BoxFuture<'_, Vec<PathBuf>> {
        async move {
            if root.is_dir() {
                let mut entries = vec![];
                let mut read_dir = fs::read_dir(root).await.unwrap_or_else(|err| {
                    panic!("Could not read directory: {:?} due to {:?}", root, err)
                });
                while let Ok(Some(entry)) = read_dir.next_entry().await {
                    let path = entry.path();
                    let mut rest = if path.is_dir() {
                        LocalSandbox::scan_files(&path).await
                    } else {
                        vec![path]
                    };
                    entries.append(&mut rest);
                }
                entries
            } else {
                vec![]
            }
        }
        .boxed()
    }

    pub async fn all_outputs(&self) -> Vec<PathBuf> {
        LocalSandbox::scan_files(&self.root)
            .await
            .iter()
            .flat_map(|path| path.strip_prefix(&self.root))
            .map(|path| path.to_path_buf())
            .collect()
    }

    /// Update the validation status of this sandbox by comparing the expected
    /// and actual outputs of this build rule.
    ///
    /// It is at this point that we enforce that every rule in the system will
    /// only create the outputs that it has declared.
    ///
    /// No undeclared outputs will be accepted, as they may collide with transitive
    /// dependency outputs that will be present in the sandbox.
    ///
    #[tracing::instrument(name = "LocalSandbox::validate_outputs", skip(self))]
    async fn validate_outputs(&mut self) -> Result<(), anyhow::Error> {
        let node_inputs: FxHashSet<PathBuf> = self.node.srcs().iter().cloned().collect();
        let deps_inputs: FxHashSet<PathBuf> = self
            .node
            .deps()
            .iter()
            .flat_map(|n| n.outs.clone())
            .collect();

        let inputs: FxHashSet<PathBuf> = node_inputs.union(&deps_inputs).cloned().collect();

        let expected_outputs: FxHashSet<PathBuf> = self.node.outs().iter().cloned().collect();

        let all_outputs: FxHashSet<PathBuf> = LocalSandbox::scan_files(&self.root)
            .await
            .iter()
            .flat_map(|path| path.strip_prefix(&self.root))
            .map(|path| path.to_path_buf())
            .collect();

        // No outputs either expected or created, what did this rule do anyway?
        if expected_outputs.is_empty() || all_outputs.is_empty() {
            self.status = ValidationStatus::NoOutputs;
            return Ok(());
        }

        // All files found minus Inputs
        let actual_outputs: FxHashSet<PathBuf> = all_outputs
            .iter()
            .filter(|path| {
                !inputs.contains(&path.to_path_buf())
                    || expected_outputs.contains(&path.to_path_buf())
            })
            .cloned()
            .collect();

        let no_difference = expected_outputs
            .difference(&actual_outputs)
            .next()
            .is_none();

        // No diff means we have the outputs we expected!
        if no_difference {
            self.status = ValidationStatus::Valid;
            self.outputs = expected_outputs.iter().cloned().collect();
        } else {
            let expected_but_missing = expected_outputs
                .difference(&actual_outputs)
                .cloned()
                .collect();
            let expected_and_present = actual_outputs
                .intersection(&expected_outputs)
                .cloned()
                .collect();

            let mut unexpected_but_present = vec![];
            for actual in actual_outputs {
                if !expected_outputs.contains(&actual) {
                    unexpected_but_present.push(actual.clone())
                }
            }

            self.status = ValidationStatus::Invalid {
                expected_and_present,
                unexpected_but_present,
                expected_but_missing,
            }
        }

        Ok(())
    }

    #[tracing::instrument(name = "LocalSandbox::prepare_sandbox_dir", skip(self))]
    async fn prepare_sandbox_dir(&mut self) -> Result<PathBuf, anyhow::Error> {
        let current_dir = std::env::current_dir().context(format!(
            "Could not get the current directory while building sandbox for node {:?}",
            self.node.label().to_string(),
        ))?;

        let _ = fs::remove_dir_all(&self.root).await;
        fs::create_dir_all(&self.root)
            .await
            .context(format!(
                "Could not create sandbox directory for node {:?} at: {:?}",
                self.node.label().to_string(),
                &self.root
            ))
            .map(|_| ())?;

        debug!("Created sandbox at: {:?}", &self.root);

        Ok(current_dir)
    }

    #[tracing::instrument(name = "LocalSandbox::copy_dependences", skip(self, find_node))]
    async fn copy_dependences(
        &mut self,
        build_cache: &LocalCache,
        find_node: &dyn Fn(Label) -> Option<ComputedTarget>,
    ) -> Result<(), error::SandboxError> {
        // copy all the direct dependency outputs
        for dep in self
            .node
            .transitive_deps(find_node)
            .map_err(error::SandboxError::MissingDependencies)?
            .iter()
        {
            for out in dep.outs.iter() {
                let src = build_cache
                    .absolute_path_by_hash(&dep.hash)
                    .await
                    .map_err(error::SandboxError::Unknown)?
                    .join(&out);
                let dst = self.root.join(&out);
                self.copy_file(&src, &dst).await?;
            }
        }

        Ok(())
    }

    #[tracing::instrument(name="LocalSandbox::copy_file", skip(self), fields(warp.target = %self.node().label().to_string()))]
    async fn copy_file(&self, src: &PathBuf, dst: &PathBuf) -> Result<(), error::SandboxError> {
        if let Some(dst_parent) = &dst.parent() {
            fs::create_dir_all(dst_parent)
                .await
                .map_err(|_| error::SandboxError::CouldNotCreateDir {
                    dst: dst.clone(),
                    dst_parent: dst_parent.to_path_buf(),
                })
                .map(|_| ())?;
        };

        #[cfg(target_os = "windows")]
        let link_result = match std::os::windows::fs::symlink(&src, &dst) {
            Ok(_) => Ok(()),
            Err(err) if err.kind() == std::io::ErrorKind::AlreadyExists => Ok(()),
            err => err,
        };

        #[cfg(not(target_os = "windows"))]
        let link_result = match std::os::unix::fs::symlink(&src, &dst) {
            Ok(_) => Ok(()),
            Err(err) if err.kind() == std::io::ErrorKind::AlreadyExists => Ok(()),
            err => err,
        };

        link_result.map_err(|_| error::SandboxError::CouldNotLink {
            label: self.node.label().clone(),
            src: src.clone(),
            dst: dst.clone(),
        })?;

        Ok(())
    }

    #[tracing::instrument(name = "LocalSandbox::copy_inputs", skip(self))]
    async fn copy_inputs(&mut self) -> Result<(), anyhow::Error> {
        let inputs = &self.node.srcs();
        for src in inputs {
            let dst = self.root.join(&src);
            let src = fs::canonicalize(&src).await?;
            self.copy_file(&src, &dst).await?;
        }
        Ok(())
    }

    #[tracing::instrument(name = "LocalSandbox::clear_sandbox", skip(self))]
    pub async fn clear_sandbox(&self) -> Result<(), anyhow::Error> {
        if fs::metadata(&self.root).await.is_ok() {
            fs::remove_dir_all(&self.root).await.context(format!(
                "Could not clean sandbox for node {:?} at {:?}",
                self.node.label().to_string(),
                &self.root,
            ))?;
        }
        Ok(())
    }

    /// check that transitive output names and current rule output names
    /// do not collide.
    #[tracing::instrument(name = "LocalSandbox::ensure_outputs_are_safe", skip(self))]
    async fn ensure_outputs_are_safe(&mut self) -> Result<(), anyhow::Error> {
        let output_set: FxHashSet<PathBuf> = self.node.outs().iter().cloned().collect();

        let dep_output_set: FxHashSet<PathBuf> = self
            .node
            .deps()
            .iter()
            .flat_map(|os| os.outs.clone())
            .collect();

        if !output_set.is_disjoint(&dep_output_set) {
            let overlapping_outputs = output_set.intersection(&dep_output_set);
            Err(anyhow!(
                "Oops, this rule would collide by creating outputs that would
          override dependency outputs: {:?}",
                overlapping_outputs
            ))
        } else {
            Ok(())
        }
    }

    #[tracing::instrument(name = "LocalSandbox::promote_outputs", skip(self))]
    async fn promote_outputs(&mut self) -> Result<(), anyhow::Error> {
        for out in &self.outputs {
            let src = self.root.join(&out);
            let dst = self.outputs_root.join(&out);

            let dst_dir = dst.parent().context("Could not find parent dir")?;
            fs::create_dir_all(&dst_dir)
                .await
                .context(format!(
                    "Could not create sandbox directory for node {:?} at: {:?}",
                    self.node.label().to_string(),
                    &self.root
                ))
                .map(|_| ())?;

            self.copy_file(&src, &dst).await?;
        }
        Ok(())
    }

    /// Run a build rule within a sandboxed environment.
    ///
    #[tracing::instrument(name = "LocalSandbox::run", skip(self, find_node))]
    pub async fn run(
        &mut self,
        build_cache: &LocalCache,
        find_node: &dyn Fn(Label) -> Option<ComputedTarget>,
        mode: ExecutionMode,
        event_channel: Arc<EventChannel>,
    ) -> Result<ValidationStatus, anyhow::Error> {
        debug!("Running sandbox at: {:?}", &self.root);

        self.ensure_outputs_are_safe().await?;

        self.prepare_sandbox_dir().await?;

        self.copy_dependences(build_cache, find_node).await?;

        self.copy_inputs().await?;

        debug!("Executing build rule...");
        self.node
            .execute(
                &self.workspace.paths.local_cache_root,
                &self.root,
                mode,
                event_channel.clone(),
            )
            .await
            .map_err(|error| error::SandboxError::ExecutionError {
                label: self.node.label().clone(),
                sandbox_root: self.root.clone(),
                error,
            })?;

        debug!("Build rule executed successfully.");

        self.validate_outputs().await?;

        if let ValidationStatus::Valid = self.status {
            self.promote_outputs().await?;
        }

        debug!(
            "Sandbox status updated to: {:?} {:?}",
            &self.status, &self.outputs
        );

        Ok(self.status.clone())
    }
}
