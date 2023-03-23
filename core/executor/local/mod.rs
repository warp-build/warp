mod context;
pub mod traced_action_runner;

pub use context::*;

use self::traced_action_runner::{ActionRunnerFlow, TracedActionRunner};
use super::{ExecutionFlow, Executor, ExecutorError, ValidationStatus};
use crate::model::{ConcreteTarget, ExecutableSpec};
use crate::store::{ArtifactManifest, BuildStamps, Store};
use crate::CacheStatus;
use async_trait::async_trait;
use futures::FutureExt;
use fxhash::FxHashSet;
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use tokio::fs;

pub struct LocalExecutor {
    ctx: LocalExecutorContext,
}

#[async_trait]
impl Executor for LocalExecutor {
    type Context = LocalExecutorContext;

    fn new(ctx: LocalExecutorContext) -> Result<Self, ExecutorError> {
        Ok(Self { ctx })
    }

    #[tracing::instrument(name = "LocalExecutor::execute", skip(self, spec))]
    async fn execute(&mut self, spec: &ExecutableSpec) -> Result<ExecutionFlow, ExecutorError> {
        if let Some(manifest) = self.ctx.artifact_store.find(spec).await? {
            self.ctx.artifact_store.promote(&manifest).await?;
            return Ok(ExecutionFlow::Completed(manifest, CacheStatus::Cached));
        }

        self.do_execute(spec).await
    }
}

impl LocalExecutor {
    async fn do_execute(&self, spec: &ExecutableSpec) -> Result<ExecutionFlow, ExecutorError> {
        let build_started_at = chrono::Utc::now();

        self.ctx.artifact_store.clean(spec).await?;

        let store_path = self.ctx.artifact_store.get_local_store_path_for_spec(spec);

        let shell_env = self.shell_env(&store_path, spec);
        self.copy_files(&store_path, spec).await?;

        if let ActionRunnerFlow::MissingInputs { inputs } =
            TracedActionRunner::run(&store_path, &shell_env, spec).await?
        {
            let deps = vec![];
            for _input in inputs {}
            return Ok(ExecutionFlow::MissingDeps { deps });
        }

        match self.validate_outputs(&store_path, spec).await? {
            ValidationStatus::Valid { .. } => {
                let build_completed_at = chrono::Utc::now();

                let buildstamps = BuildStamps {
                    plan_started_at: spec.planning_start_time(),
                    plan_ended_at: spec.planning_end_time(),
                    plan_elapsed_time: spec
                        .planning_end_time()
                        .signed_duration_since(spec.planning_start_time()),
                    build_completed_at,
                    build_started_at,
                    build_elapsed_time: build_completed_at.signed_duration_since(build_started_at),
                };

                let deps = spec
                    .deps()
                    .compile_deps()
                    .iter()
                    .map(|dep| {
                        let manifest = self
                            .ctx
                            .task_results
                            .get_task_result(*dep)
                            .unwrap()
                            .artifact_manifest;
                        (manifest.target().to_string(), manifest.hash().to_string())
                    })
                    .collect();

                let runtime_deps = spec
                    .deps()
                    .runtime_deps()
                    .iter()
                    .map(|dep| {
                        let manifest = self
                            .ctx
                            .task_results
                            .get_task_result(*dep)
                            .unwrap()
                            .artifact_manifest;
                        (manifest.target().to_string(), manifest.hash().to_string())
                    })
                    .collect();

                let transitive_deps = spec
                    .deps()
                    .transitive_deps()
                    .iter()
                    .map(|dep| {
                        let manifest = self
                            .ctx
                            .task_results
                            .get_task_result(*dep)
                            .unwrap()
                            .artifact_manifest;
                        (manifest.target().to_string(), manifest.hash().to_string())
                    })
                    .collect();

                let toolchains = spec
                    .deps()
                    .toolchains()
                    .iter()
                    .map(|dep| {
                        let manifest = self
                            .ctx
                            .task_results
                            .get_task_result(*dep)
                            .unwrap()
                            .artifact_manifest;
                        (manifest.target().to_string(), manifest.hash().to_string())
                    })
                    .collect();

                let manifest = ArtifactManifest::build_v0()
                    .target(spec.target().to_string())
                    .rule_name(spec.signature().rule().to_string())
                    .hash(spec.hash().to_string())
                    .srcs(spec.srcs().files().iter().cloned().collect())
                    .outs(spec.outs().files().iter().cloned().collect())
                    .deps(deps)
                    .runtime_deps(runtime_deps)
                    .transitive_deps(transitive_deps)
                    .toolchains(toolchains)
                    .provides(spec.provides().files().clone())
                    .exec_env(spec.exec_env().clone().into())
                    .shell_env(shell_env)
                    .store_path(store_path)
                    .buildstamps(buildstamps)
                    .build()?;

                self.ctx.artifact_store.promote(&manifest).await?;
                self.ctx.artifact_store.save(spec, &manifest).await?;

                Ok(ExecutionFlow::Completed(manifest, CacheStatus::Fresh))
            }
            validation => Ok(ExecutionFlow::ValidationError(validation)),
        }
    }

    fn shell_env(&self, store_path: &Path, spec: &ExecutableSpec) -> BTreeMap<String, String> {
        let mut env = BTreeMap::default();
        for toolchain in spec.deps().toolchains().iter().map(|l| {
            self.ctx
                .task_results
                .get_task_result(*l)
                .unwrap()
                .artifact_manifest
        }) {
            env.extend(toolchain.shell_env().clone())
        }

        for dep in spec.deps().compile_deps().iter().map(|l| {
            self.ctx
                .task_results
                .get_task_result(*l)
                .unwrap()
                .artifact_manifest
        }) {
            env.extend(dep.shell_env().clone())
        }

        for dep in spec.deps().transitive_deps().iter().map(|l| {
            self.ctx
                .task_results
                .get_task_result(*l)
                .unwrap()
                .artifact_manifest
        }) {
            env.extend(dep.shell_env().clone())
        }

        let store_path_str = store_path.to_str().unwrap();
        for (name, value) in spec.shell_env() {
            let value = value.replace("{{NODE_STORE_PATH}}", store_path_str);
            env.insert(name.clone(), value);
        }

        let spec_paths = spec.provides().values().clone().map(|p| store_path.join(p));

        let paths: FxHashSet<String> = spec
            .deps()
            .toolchains()
            .iter()
            .chain(spec.deps().transitive_deps().iter())
            .chain(spec.deps().runtime_deps().iter())
            .chain(spec.deps().compile_deps().iter())
            .map(|l| {
                self.ctx
                    .task_results
                    .get_task_result(*l)
                    .unwrap()
                    .artifact_manifest
            })
            .flat_map(|d| {
                d.provided_files()
                    .values()
                    .map(|p| d.store_path().join(p))
                    .collect::<Vec<PathBuf>>()
            })
            .chain(spec_paths)
            .map(|p| p.parent().unwrap().to_str().unwrap().to_string())
            .collect();

        let mut paths = paths.into_iter().collect::<Vec<String>>();
        paths.reverse();

        env.insert("PATH".to_string(), paths.join(":"));
        env.insert(
            "WARP_BOOTSTRAP".to_string(),
            spec.goal().is_bootstrap().to_string(),
        );
        env
    }

    async fn copy_files(
        &self,
        store_path: &Path,
        spec: &ExecutableSpec,
    ) -> Result<(), ExecutorError> {
        // Copy dependencies
        for dep in spec
            .deps()
            .transitive_deps()
            .iter()
            .chain(spec.deps().runtime_deps().iter())
            .map(|d| {
                self.ctx
                    .task_results
                    .get_task_result(*d)
                    .unwrap()
                    .artifact_manifest
            })
        {
            let dep_src = self
                .ctx
                .artifact_store
                .get_local_store_path_for_manifest(&dep);

            for out in dep.outs().iter() {
                let src = dep_src.join(out);
                let dst = store_path.join(out);
                self.copy_file(spec.target(), &src, &dst).await?;
            }
        }

        // Copy sources
        for src in spec.srcs().files() {
            let dst = store_path.join(src);
            let src = spec.target().workspace_root().join(src);
            if src.eq(&dst) {
                panic!(
                    r#"We almost copied a file onto itself!

SRC = {:?}
DST = {:?}
STORE_PATH = {:?}

spec = {:#?}

"#,
                    src, dst, store_path, spec
                );
            }
            self.copy_file(spec.target(), &src, &dst).await?;
        }

        Ok(())
    }

    async fn validate_outputs(
        &self,
        store_path: &PathBuf,
        spec: &ExecutableSpec,
    ) -> Result<ValidationStatus, ExecutorError> {
        let node_inputs: &FxHashSet<PathBuf> = spec.srcs().files();

        let deps_inputs: FxHashSet<PathBuf> = spec
            .deps()
            .compile_deps()
            .iter()
            .map(|l| {
                self.ctx
                    .task_results
                    .get_task_result(*l)
                    .unwrap()
                    .artifact_manifest
            })
            .flat_map(|n| n.outs().to_vec())
            .collect();

        let inputs: FxHashSet<PathBuf> = node_inputs.union(&deps_inputs).cloned().collect();

        let all_outputs: FxHashSet<PathBuf> = scan_files(store_path)
            .await
            .iter()
            .flat_map(|path| path.strip_prefix(store_path))
            .map(|path| path.to_path_buf())
            .collect();

        // NOTE(@ostera): if a directory has been specified as an output, we'll just expand it
        // to all the _actual_ files that live within that folder. This is an antipattern, because
        // it means that we don't know what the outputs of a rule will be until the rule is run.
        //
        // It is, however, _useful_ for rules that generate large amounts of outputs, such as lower
        // level C-style libraries (think OpenSSL).
        //
        let mut expected_outputs: FxHashSet<PathBuf> = FxHashSet::default();
        for out in spec.outs().files() {
            let abs_out = store_path.join(out);
            if abs_out.is_dir() {
                for path in scan_files(&abs_out).await {
                    let path = path
                        .strip_prefix(&store_path.join("."))
                        .unwrap()
                        .to_path_buf();
                    expected_outputs.insert(path);
                }
            } else {
                let out = if out.starts_with(".") {
                    out.strip_prefix(".").unwrap()
                } else {
                    out
                };
                expected_outputs.insert(out.to_path_buf());
            }
        }

        // NOTE(@ostera): no expected or actual outputs, so lets bail early!
        if expected_outputs.is_empty() || all_outputs.is_empty() {
            return Ok(ValidationStatus::Valid { outputs: vec![] });
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
            Ok(ValidationStatus::Valid {
                outputs: expected_outputs.iter().cloned().collect(),
            })
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

            Ok(ValidationStatus::Invalid {
                store_path: store_path.to_path_buf(),
                expected_and_present,
                unexpected_but_present,
                expected_but_missing,
            })
        }
    }

    #[tracing::instrument(name = "LocalExecutor::copy_file", skip(self))]
    async fn copy_file(
        &self,
        target: &ConcreteTarget,
        src: &PathBuf,
        dst: &PathBuf,
    ) -> Result<(), ExecutorError> {
        if let Some(dst_parent) = &dst.parent() {
            fs::create_dir_all(dst_parent)
                .await
                .map_err(|_| ExecutorError::CouldNotCreateDir {
                    target: target.clone().into(),
                    dst: dst.clone(),
                    dst_parent: dst_parent.to_path_buf(),
                })
                .map(|_| ())?;
        };

        match fs::copy(&src, &dst).await.map(|_| ()) {
            Ok(_) => (),
            Err(err) if err.kind() == std::io::ErrorKind::InvalidInput => (),
            Err(err) if err.kind() == std::io::ErrorKind::AlreadyExists => (),
            Err(err) => {
                return Err(ExecutorError::CouldNotCopy {
                    target: target.clone().into(),
                    src: src.clone(),
                    dst: dst.clone(),
                    err,
                })
            }
        };

        Ok(())
    }
}

fn scan_files(root: &'_ PathBuf) -> futures::future::BoxFuture<'_, Vec<PathBuf>> {
    async move {
        if root.is_dir() {
            let mut entries = vec![];
            let mut read_dir = fs::read_dir(root).await.unwrap_or_else(|err| {
                panic!("Could not read directory: {:?} due to {:?}", root, err)
            });
            while let Ok(Some(entry)) = read_dir.next_entry().await {
                let path = entry.path();
                let mut rest = if path.is_dir() {
                    scan_files(&path).await
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
