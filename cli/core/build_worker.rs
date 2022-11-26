use super::Event;
use super::*;
use std::path::PathBuf;
use std::sync::Arc;
use thiserror::*;
use tracing::*;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Role {
    MainWorker,
    HelperWorker(usize),
}

impl Role {
    pub fn is_main_worker(&self) -> bool {
        matches!(&self, Role::MainWorker)
    }
}

#[derive(Error, Debug)]
pub enum BuildWorkerError {
    #[error(transparent)]
    QueueError(build_queue::QueueError),

    #[error(transparent)]
    LabelResolverError(LabelResolverError),

    #[error(transparent)]
    TargetPlannerError(TargetPlannerError),

    #[error("Node {} expected the following but missing outputs: {:?}\n\nInstead it found the following unexpected outputs: {:?}", label.to_string(), expected_but_missing, unexpected_but_present)]
    TargetFailedValidation {
        label: Label,
        expected_but_missing: Vec<PathBuf>,
        unexpected_but_present: Vec<PathBuf>,
        expected_and_present: Vec<PathBuf>,
    },
}

pub struct BuildWorker {
    pub role: Role,
    pub coordinator: Arc<BuildCoordinator>,
    pub event_channel: Arc<EventChannel>,
    pub build_opts: BuildOpts,

    /// The queue from which workers pull work.
    pub build_queue: Arc<BuildQueue>,
    pub build_results: Arc<BuildResults>,
    pub label_registry: Arc<LabelRegistry>,
    pub label_resolver: Arc<LabelResolver>,
    pub target_executor: Arc<TargetExecutor>,

    pub env: ExecutionEnvironment,

    pub target_planner: TargetPlanner,
}

impl BuildWorker {
    pub fn new(
        role: Role,
        coordinator: Arc<BuildCoordinator>,
        event_channel: Arc<EventChannel>,
        build_queue: Arc<BuildQueue>,
        build_results: Arc<BuildResults>,
        label_registry: Arc<LabelRegistry>,
        label_resolver: Arc<LabelResolver>,
        target_executor: Arc<TargetExecutor>,
        artifact_store: Arc<ArtifactStore>,
        share_rule_executor_state: Arc<SharedRuleExecutorState>,
        source_manager: Arc<SourceManager>,
        build_opts: BuildOpts,
    ) -> Result<Self, BuildWorkerError> {
        let env = ExecutionEnvironment::new();

        let target_planner = TargetPlanner::new(
            build_results.clone(),
            artifact_store,
            source_manager,
            share_rule_executor_state,
            label_registry.clone(),
        )
        .map_err(BuildWorkerError::TargetPlannerError)?;

        Ok(Self {
            build_queue,
            build_results,
            coordinator,
            env,
            event_channel,
            label_registry,
            label_resolver,
            role,
            target_executor,
            target_planner,
            build_opts,
        })
    }

    #[tracing::instrument(name = "BuildWorker::setup_and_run", skip(self))]
    pub async fn setup_and_run(&mut self) -> Result<(), BuildWorkerError> {
        loop {
            // NOTE(@ostera): we don't want things to burn CPU cycles
            tokio::time::sleep(std::time::Duration::from_micros(10)).await;
            let result = self.run().await;
            if result.is_err() {
                self.coordinator.signal_shutdown();
                self.finish();
                return result;
            }
            if self.should_stop() {
                self.finish();
                break;
            }
        }
        Ok(())
    }

    pub fn should_stop(&self) -> bool {
        if Role::MainWorker == self.role && self.build_results.has_all_expected_targets() {
            self.coordinator.signal_shutdown();
        }
        self.coordinator.should_shutdown()
    }

    pub fn finish(&mut self) {
        if self.role == Role::MainWorker {
            self.event_channel
                .send(Event::BuildCompleted(std::time::Instant::now()))
        }
    }

    #[tracing::instrument(name = "BuildWorker::run", skip(self))]
    pub async fn run(&mut self) -> Result<(), BuildWorkerError> {
        let next_task = self.build_queue.next();

        if next_task.is_none() {
            return Ok(());
        }
        let task = next_task.unwrap();
        let label = task.label;

        self.event_channel.send(Event::HandlingTarget {
            label: self.label_registry.get_label(label).as_ref().to_owned(),
            goal: task.goal,
        });

        let target = match self.label_resolver.resolve(label, task.goal).await {
            Err(LabelResolverError::DependencyResolverError(
                DependencyResolverError::MissingResolver { resolver },
            )) => {
                return self.requeue(task, &[resolver]).await;
            }

            Err(LabelResolverError::SourceManagerError(SourceManagerError::MissingParser {
                parser_id,
                ..
            })) => {
                return self.requeue(task, &[parser_id]).await;
            }

            Err(LabelResolverError::SignatureStoreError(
                SignatureStoreError::MissingGenerator { generator, .. },
            )) => {
                return self.requeue(task, &[generator]).await;
            }

            Err(LabelResolverError::DependencyResolverError(
                DependencyResolverError::ResolverServiceManagerError(
                    ResolverServiceManagerError::MissingService(service_id),
                ),
            ))
            | Err(LabelResolverError::SourceManagerError(
                SourceManagerError::AnalyzerServiceManagerError(
                    AnalyzerServiceManagerError::MissingService(service_id),
                ),
            ))
            | Err(LabelResolverError::SignatureStoreError(
                SignatureStoreError::AnalyzerServiceManagerError(
                    AnalyzerServiceManagerError::MissingService(service_id),
                ),
            )) => {
                return self.requeue(task, &[service_id]).await;
            }

            Err(LabelResolverError::SourceManagerError(
                SourceManagerError::BadSignature | SourceManagerError::UnknownParser { .. },
            )) => {
                self.build_queue.skip(task);
                return Ok(());
            }

            Err(err) => {
                self.event_channel.send(Event::BuildError {
                    label: (*self.label_registry.get_label(label)).to_owned(),
                    error: BuildError::LabelResolverError(err),
                });
                self.coordinator.signal_shutdown();
                return Ok(());
            }

            Ok(targets) => {
                let target = targets.get(0).unwrap().clone();
                let original_label = self.label_registry.get_label(label);
                if *original_label != target.label {
                    self.label_registry
                        .update_label(label, target.label.clone());
                }

                for target in targets.into_iter().skip(1) {
                    let label = self.label_registry.register_label(target.label);
                    let task = Task { label, ..task };
                    self.build_queue
                        .queue(task)
                        .map_err(BuildWorkerError::QueueError)?;
                }

                target
            }
        };

        for dep in &target.runtime_deps {
            let dep = self.label_registry.register_label(dep.to_owned());
            self.build_queue
                .queue(Task::build(dep))
                .map_err(BuildWorkerError::QueueError)?;
        }

        let executable_target = match self
            .target_planner
            .plan(&self.build_opts, &self.env, label, task.goal, &target)
            .await
        {
            Err(TargetPlannerError::MissingDependencies { deps, .. }) => {
                return self.requeue(task, &deps).await;
            }

            Err(err) => {
                self.event_channel.send(Event::BuildError {
                    label: target.label.clone(),
                    error: BuildError::TargetPlannerError(err),
                });
                self.coordinator.signal_shutdown();
                return Ok(());
            }

            Ok(executable_target) => executable_target,
        };

        if task.goal.is_fetch() {
            self.build_results.add_fetch_result(task.label);
            self.build_queue.ack(task);
            return Ok(());
        }

        self.event_channel.send(Event::BuildingTarget {
            label: target.label.clone(),
            goal: task.goal,
            rule_mnemonic: executable_target.rule.mnemonic.to_string(),
        });

        match self
            .target_executor
            .execute(&executable_target, self.build_opts)
            .await
        {
            // NOTE(@ostera): if someone else is already building this target, we'll postpone it.
            Err(TargetExecutorError::ArtifactStoreError(ArtifactStoreError::StoreLockError(
                StoreLockError::LockedKey(_key),
            ))) => self.build_queue.nack(task),

            Err(err) => {
                self.event_channel.send(Event::BuildError {
                    label: executable_target.label.clone().into(),
                    error: BuildError::TargetExecutorError(err),
                });

                self.coordinator.signal_shutdown();
            }

            Ok((
                _manifest,
                ValidationStatus::Invalid {
                    expected_and_present,
                    expected_but_missing,
                    unexpected_but_present,
                },
            )) => {
                self.event_channel.send(Event::BuildError {
                    label: executable_target.label.clone().into(),
                    error: BuildError::BuildWorkerError(BuildWorkerError::TargetFailedValidation {
                        label: executable_target.label.clone().into(),
                        expected_but_missing,
                        unexpected_but_present,
                        expected_and_present,
                    }),
                });

                self.coordinator.signal_shutdown();
            }

            Ok((manifest, _)) => {
                self.target_planner
                    .update(&executable_target)
                    .await
                    .map_err(BuildWorkerError::TargetPlannerError)?;

                let tmp_label: Label = executable_target.label.to_owned().into();
                let final_label_id = self.label_registry.register_label(tmp_label);
                let final_label = self.label_registry.get_label(final_label_id);

                if manifest.cached {
                    self.event_channel.send(Event::CacheHit {
                        label: (*final_label).clone(),
                        goal: task.goal,
                    });
                } else {
                    self.event_channel.send(Event::TargetBuilt {
                        label: (*final_label).clone(),
                        goal: task.goal,
                    });
                }

                self.build_results
                    .add_computed_target(final_label_id, manifest, executable_target);

                self.build_queue.ack(task);
            }
        }

        Ok(())
    }

    #[inline]
    async fn requeue(&self, task: Task, deps: &[LabelId]) -> Result<(), BuildWorkerError> {
        if let Err(QueueError::DependencyCycle(err)) = self.build_queue.queue_deps(task, deps) {
            self.event_channel.send(Event::BuildError {
                label: (*self.label_registry.get_label(task.label)).to_owned(),
                error: BuildError::BuildResultError(err),
            });
            self.coordinator.signal_shutdown();
        }

        self.build_queue.nack(task);

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use super::*;

    async fn make_target(label: Label) -> ExecutableTarget {
        let rule = Rule::new(
            "test_rule".to_string(),
            "TestRule".to_string(),
            vec![],
            ConfigSpec::default(),
            RuleConfig::default(),
            Runnable::NotRunnable,
            Pinned::Pinned,
            Portability::Portable,
        );
        let cfg = RuleConfig::default();
        let target = Target::new(label, &rule.name, cfg);
        ExecutableTarget::new(
            &ExecutionEnvironment::new(),
            &rule,
            &target,
            &[],
            &[],
            &[],
            ExecutionResult::default(),
        )
        .await
        .unwrap()
    }

    async fn make_manifest(target: &ExecutableTarget) -> TargetManifest {
        TargetManifest::from_validation_result(
            chrono::Utc::now(),
            &ValidationStatus::Cached,
            &PathBuf::from("."),
            BTreeMap::default(),
            target,
        )
    }

    fn worker_with_results(
        r: Role,
        br: Arc<BuildResults>,
        bc: Arc<BuildCoordinator>,
    ) -> BuildWorker {
        let w = Workspace::default();
        let l = Label::new("//hello/world");
        let ec = Arc::new(EventChannel::new());
        let bq = Arc::new(BuildQueue::new(
            l.clone(),
            br.clone(),
            ec.clone(),
            w.clone(),
        ));
        let lr = Arc::new(LabelResolver::new(&w));
        let s = Arc::new(ArtifactStore::new(&w));
        let te = Arc::new(TargetExecutor::new(s.clone(), ec.clone()));
        let rs = Arc::new(RuleStore::new(&w));
        let sres = Arc::new(SharedRuleExecutorState::new(rs.clone()));
        BuildWorker::new(r, l, bc, ec, bq, br, lr, te, s, sres).unwrap()
    }

    #[tokio::test]
    async fn when_results_are_finished_main_worker_stops_then_helpers() {
        let l = Label::new("//test/0");

        let bc = Arc::new(BuildCoordinator::new());
        let br = Arc::new(BuildResults::new());

        let main_worker = worker_with_results(Role::MainWorker, br.clone(), bc.clone());
        let help_worker = worker_with_results(Role::HelperWorker(0), br.clone(), bc.clone());

        assert!(!main_worker.should_stop());
        assert!(!help_worker.should_stop());

        br.add_expected_target(l.clone());
        let target = make_target(l.clone()).await;
        let manifest = make_manifest(&target).await;
        br.add_computed_target(l.clone(), manifest, target);

        assert!(!help_worker.should_stop());
        assert!(main_worker.should_stop());
        assert!(help_worker.should_stop());
    }
}
