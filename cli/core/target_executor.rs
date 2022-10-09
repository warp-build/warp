use super::*;
use futures::FutureExt;
use fxhash::FxHashSet;
use std::{
    collections::BTreeMap,
    path::{Path, PathBuf},
    sync::Arc,
};
use thiserror::*;
use tokio::fs;

pub struct TargetExecutor {
    event_channel: Arc<EventChannel>,
    store: Arc<Store>,
}

#[derive(Error, Debug)]
pub enum TargetExecutorError {
    #[error(transparent)]
    StoreError(StoreError),

    #[error("When building {label:?}, could not create directory {dst:?} at: {dst_parent:?}")]
    CouldNotCreateDir {
        label: Label,
        dst: PathBuf,
        dst_parent: PathBuf,
    },

    #[error("When building {label:?}, could not copy {src:?} into sandbox at {dst:?}")]
    CouldNotCopy {
        label: Label,
        src: PathBuf,
        dst: PathBuf,
    },

    #[error(transparent)]
    ActionError(anyhow::Error),
}

#[derive(Debug, Clone)]
pub enum ValidationStatus {
    Invalid {
        expected_and_present: Vec<PathBuf>,
        expected_but_missing: Vec<PathBuf>,
        unexpected_but_present: Vec<PathBuf>,
    },
    Cached,
    Valid {
        outputs: Vec<PathBuf>,
    },
}

impl TargetExecutor {
    pub fn new(store: Arc<Store>, event_channel: Arc<EventChannel>) -> Self {
        Self {
            store,
            event_channel,
        }
    }

    pub async fn execute(
        &self,
        target: &ExecutableTarget,
    ) -> Result<(TargetManifest, ValidationStatus), TargetExecutorError> {
        let build_started_at = chrono::Utc::now();

        if let StoreHitType::Hit(manifest) = self
            .store
            .is_stored(target)
            .await
            .map_err(TargetExecutorError::StoreError)?
        {
            return Ok((*manifest, ValidationStatus::Cached));
        }

        self.store
            .clean(target)
            .await
            .map_err(TargetExecutorError::StoreError)?;

        let store_path = self
            .store
            .absolute_path_by_node(target)
            .await
            .map_err(TargetExecutorError::StoreError)?;

        let env = self.shell_env(&store_path, target);

        self.copy_files(&store_path, target).await?;
        self.execute_actions(&store_path, &env, target).await?;

        let validation_result = self.validate_outputs(&store_path, target).await?;

        let manifest = TargetManifest::from_validation_result(
            build_started_at,
            &validation_result,
            &store_path,
            env,
            target,
        );

        if manifest.is_valid {
            self.store
                .promote_outputs(&manifest)
                .await
                .map_err(TargetExecutorError::StoreError)?;

            self.store
                .save(target, &manifest)
                .await
                .map_err(TargetExecutorError::StoreError)?;
        }

        Ok((manifest, validation_result))
    }

    fn shell_env(&self, store_path: &Path, target: &ExecutableTarget) -> BTreeMap<String, String> {
        let mut env = BTreeMap::default();
        for toolchain in &target.toolchains {
            env.extend(toolchain.env.clone())
        }

        for dep in &target.deps {
            env.extend(dep.env.clone())
        }

        for dep in &target.transitive_deps {
            env.extend(dep.env.clone())
        }

        let store_path_str = store_path.to_str().unwrap();
        for (name, value) in &target.env {
            let value = value.replace("{{NODE_STORE_PATH}}", store_path_str);
            env.insert(name.clone(), value);
        }

        let target_paths = target
            .provides
            .values()
            .clone()
            .into_iter()
            .map(|p| store_path.join(p));

        let paths: FxHashSet<String> = target
            .toolchains
            .iter()
            .chain(target.transitive_deps.iter())
            .chain(target.deps.iter())
            .flat_map(|d| d.provides.values())
            .cloned()
            .chain(target_paths)
            .map(|p| p.parent().unwrap().to_str().unwrap().to_string())
            .collect();

        let mut paths = paths.into_iter().collect::<Vec<String>>();
        paths.reverse();

        env.insert("PATH".to_string(), paths.join(":"));
        env
    }

    async fn copy_files(
        &self,
        store_path: &Path,
        target: &ExecutableTarget,
    ) -> Result<(), TargetExecutorError> {
        // Copy dependencies
        for dep in &target.transitive_deps {
            let dep_src = self
                .store
                .absolute_path_by_dep(dep)
                .await
                .map_err(TargetExecutorError::StoreError)?;

            for out in dep.outs.iter() {
                let src = dep_src.join(&out);
                let dst = store_path.join(&out);
                self.copy_file(&target.label, &src, &dst).await?;
            }
        }

        // Copy sources
        for src in &target.srcs {
            let dst = store_path.join(&src);
            if src.eq(&dst) {
                panic!(
                    r#"We almost copied a file onto itself!

SRC = {:?}
DST = {:?}
STORE_PATH = {:?}

TARGET = {:#?}

"#,
                    src, dst, store_path, target
                );
            }
            self.copy_file(&target.label, src, &dst).await?;
        }

        Ok(())
    }

    async fn execute_actions(
        &self,
        store_path: &PathBuf,
        env: &BTreeMap<String, String>,
        target: &ExecutableTarget,
    ) -> Result<(), TargetExecutorError> {
        self.event_channel.send(Event::PreparingActions {
            label: target.label.clone(),
            action_count: target.actions.len().try_into().unwrap(),
        });

        for action in &target.actions {
            self.event_channel.send(Event::ActionRunning {
                label: target.label.clone(),
                action: action.clone(),
            });
            action
                .run(
                    target.label.clone(),
                    store_path,
                    env,
                    self.event_channel.clone(),
                )
                .await
                .map_err(TargetExecutorError::ActionError)?
        }
        Ok(())
    }

    async fn validate_outputs(
        &self,
        store_path: &PathBuf,
        target: &ExecutableTarget,
    ) -> Result<ValidationStatus, TargetExecutorError> {
        let node_inputs: FxHashSet<PathBuf> = target.srcs.iter().cloned().collect();
        let deps_inputs: FxHashSet<PathBuf> =
            target.deps.iter().flat_map(|n| n.outs.clone()).collect();

        let inputs: FxHashSet<PathBuf> = node_inputs.union(&deps_inputs).cloned().collect();

        let all_outputs: FxHashSet<PathBuf> = self
            .scan_files(store_path)
            .await
            .iter()
            .flat_map(|path| path.strip_prefix(&store_path))
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
        for out in &target.outs {
            let abs_out = store_path.join(out);
            if abs_out.is_dir() {
                for path in self.scan_files(&abs_out).await {
                    let path = path.strip_prefix(&store_path).unwrap().to_path_buf();
                    expected_outputs.insert(path);
                }
            } else {
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
                expected_and_present,
                unexpected_but_present,
                expected_but_missing,
            })
        }
    }

    #[tracing::instrument(name = "TargetExecutor::copy_file", skip(self))]
    async fn copy_file(
        &self,
        label: &Label,
        src: &PathBuf,
        dst: &PathBuf,
    ) -> Result<(), TargetExecutorError> {
        if let Some(dst_parent) = &dst.parent() {
            fs::create_dir_all(dst_parent)
                .await
                .map_err(|_| TargetExecutorError::CouldNotCreateDir {
                    label: label.clone(),
                    dst: dst.clone(),
                    dst_parent: dst_parent.to_path_buf(),
                })
                .map(|_| ())?;
        };

        fs::copy(&label.workspace().join(src), &dst)
            .await
            .map_err(|_| TargetExecutorError::CouldNotCopy {
                label: label.clone(),
                src: src.clone(),
                dst: dst.clone(),
            })
            .map(|_| ())?;

        Ok(())
    }

    fn scan_files<'a>(&'a self, root: &'a PathBuf) -> futures::future::BoxFuture<'_, Vec<PathBuf>> {
        async move {
            if root.is_dir() {
                let mut entries = vec![];
                let mut read_dir = fs::read_dir(root).await.unwrap_or_else(|err| {
                    panic!("Could not read directory: {:?} due to {:?}", root, err)
                });
                while let Ok(Some(entry)) = read_dir.next_entry().await {
                    let path = entry.path();
                    let mut rest = if path.is_dir() {
                        self.scan_files(&path).await
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
}
