use super::Event;
use super::*;
use anyhow::anyhow;
use dashmap::DashMap;
use std::collections::HashMap;
use std::sync::Arc;
use thiserror::*;
use tracing::*;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Role {
    MainWorker,
    HelperWorker(usize),
}

#[derive(Error, Debug)]
pub enum WorkerError {
    #[error(transparent)]
    QueueError(build_queue::QueueError),

    #[error(transparent)]
    RemoteCacheError(anyhow::Error),

    #[error(transparent)]
    RuleLoadError(rule_exec_env::error::LoadError),

    #[error(transparent)]
    DepGraphError(build_results::BuildResultError),

    #[error(transparent)]
    RuleExecError(rule_exec_env::error::RuleExecError),

    #[error(transparent)]
    ComputedTargetError(computed_target::ComputedTargetError),

    #[error(transparent)]
    Unknown(anyhow::Error),
}

/// A worker executing the build.
///
pub struct BuildWorker {
    pub role: Role,

    /// The workspace this worker is currently executing.
    pub workspace: Workspace,

    pub cache: LocalCache,

    pub remote_cache: RemoteCache,

    pub rule_exec_env: RuleExecEnv,

    pub build_results: Arc<BuildResults>,

    pub coordinator: Arc<BuildCoordinator>,

    /// The queue from which workers pull work.
    pub build_queue: Arc<BuildQueue>,

    pub target: Label,

    pub target_count: usize,

    pub event_channel: Arc<EventChannel>,
}

impl BuildWorker {
    #[tracing::instrument(
        name = "BuildWorker::new",
        skip(workspace, build_queue, event_channel, toolchain_provides_map)
    )]
    pub fn new(
        role: Role,
        workspace: &Workspace,
        target: Label,
        coordinator: Arc<BuildCoordinator>,
        build_results: Arc<BuildResults>,
        build_queue: Arc<BuildQueue>,
        event_channel: Arc<EventChannel>,
        toolchain_provides_map: Arc<DashMap<Label, HashMap<String, String>>>,
    ) -> BuildWorker {
        BuildWorker {
            role,
            rule_exec_env: RuleExecEnv::new(&workspace, toolchain_provides_map),
            cache: LocalCache::new(&workspace),
            remote_cache: RemoteCache::new(&workspace),
            workspace: workspace.clone(),
            coordinator,
            build_results,
            build_queue,
            event_channel,
            target,
            target_count: 0,
        }
    }

    #[tracing::instrument(name = "BuildWorker::run", skip(self))]
    pub async fn setup_and_run(
        &mut self,
        mode: ExecutionMode,
        max_concurrency: usize,
    ) -> Result<(), WorkerError> {
        let result = {
            self.setup(max_concurrency).await?;
            loop {
                // NOTE(@ostera): we don't want things to burn CPU cycles
                tokio::time::sleep(std::time::Duration::from_millis(1)).await;
                self.run(mode).await?;
                if self.should_stop() {
                    break;
                }
            }
            Ok(())
        };

        if result.is_err() {
            self.coordinator.signal_shutdown();
        }
        self.finish();

        result
    }

    pub fn should_stop(&self) -> bool {
        if Role::MainWorker == self.role && self.build_results.has_all_expected_targets() {
            self.coordinator.signal_shutdown();
        }
        self.coordinator.should_shutdown()
    }

    pub async fn setup(&mut self, max_concurrency: usize) -> Result<(), WorkerError> {
        if self.role == Role::MainWorker {
            if self.target.is_all() {
                self.build_queue
                    .queue_entire_workspace(max_concurrency, &self.workspace, &self.rule_exec_env)
                    .await
            } else {
                self.build_queue.queue(self.target.clone())
            }
            .map_err(WorkerError::QueueError)
        } else {
            Ok(())
        }
    }

    pub fn finish(&mut self) {
        if self.role == Role::MainWorker {
            self.event_channel
                .send(Event::BuildCompleted(std::time::Instant::now()))
        }
    }

    pub async fn run(&mut self, mode: ExecutionMode) -> Result<(), WorkerError> {
        if let Some(label) = self.build_queue.next() {
            let result = match self.execute(&label, mode).await {
                Ok(_node_label) => {
                    self.build_queue.ack(&label);
                    Ok(())
                }

                Err(WorkerError::ComputedTargetError(
                    computed_target::ComputedTargetError::MissingDependencies { deps, .. },
                ))
                | Err(WorkerError::RuleExecError(
                    rule_exec_env::error::RuleExecError::MissingDependencies(
                        computed_target::ComputedTargetError::MissingDependencies { deps, .. },
                    ),
                )) => {
                    if let Err(err) = self
                        .build_results
                        .add_dependencies(label.clone(), &deps)
                        .map_err(WorkerError::DepGraphError)
                    {
                        self.event_channel
                            .send(Event::BuildError(label.clone(), err));
                        self.coordinator.signal_shutdown();
                    };

                    for dep in deps {
                        self.build_queue
                            .queue(dep)
                            .map_err(WorkerError::QueueError)?;
                    }
                    self.build_queue.nack(label.clone());
                    Ok(())
                }
                Err(err) => Err(err),
            };

            if let Err(err) = result {
                self.event_channel
                    .send(Event::BuildError(label.clone(), err));
                self.coordinator.signal_shutdown();
            }
        }
        Ok(())
    }

    #[tracing::instrument(name = "BuildWorker::load_rules", skip(self))]
    pub async fn load_rules(&mut self) -> Result<(), WorkerError> {
        self.rule_exec_env
            .setup()
            .map_err(WorkerError::RuleExecError)?;

        let built_in_rules = warp_ext::TOOLCHAINS
            .iter()
            .chain(warp_ext::RULES.iter())
            .map(|(name, src)| (name.to_string(), src.to_string()));

        let custom_rules = (self.workspace.local_toolchains.iter())
            .chain(self.workspace.local_rules.iter())
            .map(|rule| {
                let name = format!("file://{}", rule.to_str().unwrap());
                let src = std::fs::read_to_string(rule).unwrap();
                (name, src)
            });

        let mut errored = false;
        for (name, src) in built_in_rules.chain(custom_rules) {
            let load_result = self
                .rule_exec_env
                .load(&name, Some(src))
                .await
                .map_err(WorkerError::RuleLoadError);

            if let Err(err) = load_result {
                self.event_channel.send(Event::ErrorLoadingRule(name, err));
                errored = true;
                continue;
            }
        }

        if errored {
            self.coordinator.signal_shutdown();
        }

        Ok(())
    }

    #[tracing::instrument(name = "BuildWorker::execute", skip(self))]
    pub async fn execute(
        &mut self,
        label: &Label,
        mode: ExecutionMode,
    ) -> Result<Label, WorkerError> {
        let toolchain = {
            let toolchains = (*self.rule_exec_env.toolchain_manager).read().unwrap();
            toolchains.get(label)
        };
        let target = if let Some(toolchain) = toolchain {
            toolchain
        } else {
            let buildfile = Buildfile::from_file(
                &self.workspace.paths.workspace_root,
                &self
                    .workspace
                    .paths
                    .workspace_root
                    .join(label.path())
                    .join(buildfile::WARPFILE),
                self.rule_exec_env.rule_map.clone(),
            )
            .map_err(WorkerError::Unknown)?;

            buildfile
                .targets
                .iter()
                .find(|t| *t.label() == *label)
                .ok_or_else(|| anyhow!("Could not find target in: {:?}", &label.path()))
                .map_err(WorkerError::Unknown)?
                .clone()
        };
        self.execute_target(target, mode).await
    }

    #[tracing::instrument(name="BuildWorker::execute_target", skip(self), fields(warp.target = %target.label().to_string()))]
    pub async fn execute_target(
        &mut self,
        target: Target,
        mode: ExecutionMode,
    ) -> Result<Label, WorkerError> {
        let label = target.label().clone();

        self.rule_exec_env.clear();

        let find_node = |label| self.build_results.get_computed_target(&label);
        let computed_target = ComputedTarget::from_target_with_deps(target, &find_node)
            .map_err(WorkerError::ComputedTargetError)?;

        let node = self
            .rule_exec_env
            .compute_target(computed_target, &find_node)
            .map_err(WorkerError::RuleExecError)?;

        let name = node.label().clone();

        self.event_channel.send(Event::BuildingTarget {
            label: label.clone(),
            rule_mnemonic: node.target.rule().mnemonic().to_string(),
        });

        if let CacheHitType::Miss { .. } = self
            .cache
            .is_cached(&node)
            .await
            .map_err(WorkerError::Unknown)?
        {
            let _ = self.remote_cache.try_fetch(&node).await;
        }

        match self
            .cache
            .is_cached(&node)
            .await
            .map_err(WorkerError::Unknown)?
        {
            CacheHitType::Global(_cache_path) => {
                self.rule_exec_env
                    .update_provide_map(&node, &self.cache)
                    .await
                    .map_err(WorkerError::Unknown)?;

                self.event_channel.send(Event::CacheHit(label.clone()));

                self.build_results
                    .add_computed_target(label.clone(), node.clone());

                return Ok(label.clone());
            }
            CacheHitType::Local(_cache_path) => {
                debug!("Skipping {}, but promoting outputs.", name.to_string());
                self.rule_exec_env
                    .update_provide_map(&node, &self.cache)
                    .await
                    .map_err(WorkerError::Unknown)?;

                self.event_channel.send(Event::CacheHit(label.clone()));

                self.cache
                    .promote_outputs(&node, &self.workspace.paths.local_outputs_root)
                    .await
                    .map_err(WorkerError::Unknown)?;

                self.build_results
                    .add_computed_target(label.clone(), node.clone());

                return Ok(label.clone());
            }
            CacheHitType::Miss { .. } => (),
        }

        let result = {
            let mut sandbox = LocalSandbox::for_node(&self.workspace, node.clone());

            let result = {
                let find_node = |label| self.build_results.get_computed_target(&label);
                sandbox
                    .run(&self.cache, &find_node, mode, self.event_channel.clone()).await
                    .map_err(WorkerError::Unknown)?
            };

            match result {
                ValidationStatus::Valid => {
                    self.build_results.add_computed_target(label.clone(), node.clone());

                    self.cache.save(&sandbox).await.map_err(WorkerError::Unknown)?;
                   let _ =  self.remote_cache.save(&sandbox).await;

                    self.rule_exec_env.update_provide_map(&node, &self.cache)
                    .await
                    .map_err(WorkerError::Unknown)?;

                    self.cache
                        .promote_outputs(&node, &self.workspace.paths.local_outputs_root)
                        .await
                        .map_err(WorkerError::Unknown)?;

                    Ok(label.clone())
                },
                ValidationStatus::NoOutputs => {
                    if node.outs().is_empty() {
                        self.cache.save(&sandbox).await.map_err(WorkerError::Unknown)?;
                        let _ = self.remote_cache.save(&sandbox).await;
                        self.rule_exec_env.update_provide_map(&node, &self.cache).await.map_err(WorkerError::Unknown)?;
                        self.build_results.add_computed_target(label.clone(), node.clone());
                        Ok(label.clone())
                    } else {
                        Err(anyhow!( "Expected {} outputs, but found none.", node.outs().len()))
                    }
                },
                ValidationStatus::Pending => Err(anyhow!(
                    "Node {} is somehow still pending...",
                    &name.to_string()
                )),
                ValidationStatus::Invalid {
                    expected_but_missing,
                    unexpected_but_present,
                    ..
                } => Err(
                    anyhow!("Node {} expected the following but missing outputs: {:?}\n\ninstead it found the following unexpected outputs: {:?}",
                        &name.to_string(), expected_but_missing, unexpected_but_present)),
            }

        }.map_err(WorkerError::Unknown);

        self.event_channel.send(Event::TargetBuilt(label.clone()));

        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn computed_target(label: &Label) -> ComputedTarget {
        ComputedTarget {
            target: Target::new(
                label.clone(),
                &Rule::new(
                    "default_rule".to_string(),
                    "Defaultrule".to_string(),
                    vec![],
                    ConfigSpec::default(),
                    RuleConfig::default(),
                    Runnable::NotRunnable,
                    Pinned::Unpinned,
                ),
                RuleConfig::default(),
            ),
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

    #[test]
    fn main_worker_finishes_when_build_results_have_all_expected_results() {
        let final_target = Label::new("//...");
        let ec = Arc::new(EventChannel::new());
        let br = Arc::new(BuildResults::new());
        let bc = Arc::new(BuildCoordinator::new());
        let q = Arc::new(BuildQueue::new(
            final_target.clone(),
            br.clone(),
            ec.clone(),
        ));
        let w = BuildWorker::new(
            Role::MainWorker,
            &Workspace::default(),
            final_target,
            bc,
            br.clone(),
            q.clone(),
            ec,
            Arc::new(DashMap::new()),
        );

        let label1 = Label::new("//test/1");
        let label2 = Label::new("//test/2");

        let target1 = computed_target(&label1);
        let target2 = computed_target(&label2);

        q.queue(label1.clone()).unwrap();
        q.queue(label2.clone()).unwrap();

        assert!(!w.should_stop());

        q.next().unwrap();
        q.next().unwrap();

        // queue is consumed, but we still won't stop
        assert!(!w.should_stop());

        // results are submitted, now we stop!
        br.add_computed_target(label1, target1);
        assert!(!w.should_stop());
        br.add_computed_target(label2, target2);
        assert!(w.should_stop());
    }

    #[test]
    fn helper_finishes_when_the_coordinator_signals_shutdown() {
        let final_target = Label::new("//...");
        let ec = Arc::new(EventChannel::new());
        let br = Arc::new(BuildResults::new());
        let bc = Arc::new(BuildCoordinator::new());
        let q = Arc::new(BuildQueue::new(
            final_target.clone(),
            br.clone(),
            ec.clone(),
        ));
        let w = BuildWorker::new(
            Role::HelperWorker(1),
            &Workspace::default(),
            final_target.clone(),
            bc.clone(),
            br.clone(),
            q,
            ec,
            Arc::new(DashMap::new()),
        );

        assert!(!w.should_stop());

        bc.signal_shutdown();

        assert!(w.should_stop());
    }
}
