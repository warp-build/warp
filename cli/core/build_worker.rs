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

#[derive(Error, Debug)]
pub enum BuildWorkerError {
    #[error(transparent)]
    QueueError(build_queue::QueueError),

    #[error(transparent)]
    LabelResolverError(LabelResolverError),

    #[error(transparent)]
    TargetPlannerError(TargetPlannerError),

    #[error("Node {} expected the following but missing outputs: {:?}\n\ninstead it found the following unexpected outputs: {:?}", label.to_string(), expected_but_missing, unexpected_but_present)]
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

    /// The queue from which workers pull work.
    pub target: Label,
    pub build_queue: Arc<BuildQueue>,
    pub build_results: Arc<BuildResults>,
    pub label_resolver: Arc<LabelResolver>,
    pub target_executor: Arc<TargetExecutor>,

    pub env: ExecutionEnvironment,

    pub target_planner: TargetPlanner,
}

impl BuildWorker {
    pub fn new(
        role: Role,
        workspace: &Workspace,
        target: Label,
        coordinator: Arc<BuildCoordinator>,
        event_channel: Arc<EventChannel>,
        build_queue: Arc<BuildQueue>,
        build_results: Arc<BuildResults>,
        label_resolver: Arc<LabelResolver>,
        target_executor: Arc<TargetExecutor>,
        store: Arc<Store>,
        share_rule_executor_state: Arc<SharedRuleExecutorState>,
    ) -> Result<Self, BuildWorkerError> {
        let env = ExecutionEnvironment::new();

        let target_planner = TargetPlanner::new(
            workspace,
            build_results.clone(),
            store,
            share_rule_executor_state,
        )
        .map_err(BuildWorkerError::TargetPlannerError)?;

        Ok(Self {
            build_queue,
            build_results,
            coordinator,
            env,
            event_channel,
            label_resolver,
            role,
            target,
            target_executor,
            target_planner,
        })
    }

    #[tracing::instrument(name = "BuildWorker::setup_and_run", skip(self))]
    pub async fn setup_and_run(&mut self, max_concurrency: usize) -> Result<(), BuildWorkerError> {
        self.setup(max_concurrency).await?;

        loop {
            // NOTE(@ostera): we don't want things to burn CPU cycles
            tokio::time::sleep(std::time::Duration::from_micros(100)).await;
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

    #[tracing::instrument(name = "BuildWorker::setup", skip(self))]
    pub async fn setup(&mut self, max_concurrency: usize) -> Result<(), BuildWorkerError> {
        if self.role == Role::MainWorker {
            if self.target.is_all() {
                self.build_queue
                    .queue_entire_workspace(max_concurrency)
                    .await
            } else {
                self.build_queue.queue(self.target.clone())
            }
            .map_err(BuildWorkerError::QueueError)
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

    #[tracing::instrument(name = "BuildWorker::run", skip(self))]
    pub async fn run(&mut self) -> Result<(), BuildWorkerError> {
        if let Some(label) = self.build_queue.next() {
            let result = self.run_target(label).await;
            result?;
        }
        Ok(())
    }

    #[tracing::instrument(name = "BuildWorker::run_target", skip(self))]
    pub async fn run_target(&mut self, label: Label) -> Result<(), BuildWorkerError> {
        let target = self
            .label_resolver
            .resolve(&label)
            .await
            .map_err(BuildWorkerError::LabelResolverError)?;

        let executable_target = match self.target_planner.plan(&self.env, &target).await {
            Err(TargetPlannerError::MissingDependencies { deps, .. }) => {
                if let Err(QueueError::DependencyCycle(err)) =
                    self.build_queue.queue_deps(&label, &deps)
                {
                    self.event_channel.send(Event::BuildError(
                        label.clone(),
                        BuildError::BuildResultError(err),
                    ));
                    self.coordinator.signal_shutdown();
                }

                self.build_queue.nack(label.clone());
                return Ok(());
            }

            Err(err) => {
                self.event_channel.send(Event::BuildError(
                    label.clone(),
                    BuildError::TargetPlannerError(err),
                ));
                self.coordinator.signal_shutdown();
                return Ok(());
            }

            Ok(executable_target) => executable_target,
        };

        self.event_channel.send(Event::BuildingTarget {
            label: label.clone(),
            rule_mnemonic: executable_target.rule.mnemonic.to_string(),
        });

        match self.target_executor.execute(&executable_target).await {
            Err(err) => {
                self.event_channel.send(Event::BuildError(
                    label.clone(),
                    BuildError::TargetExecutorError(err),
                ));

                self.coordinator.signal_shutdown();
            }

            Ok(ValidationStatus::Invalid {
                expected_and_present,
                expected_but_missing,
                unexpected_but_present,
            }) => {
                self.event_channel.send(Event::BuildError(
                    label.clone(),
                    BuildError::BuildWorkerError(BuildWorkerError::TargetFailedValidation {
                        label: label.clone(),
                        expected_but_missing,
                        unexpected_but_present,
                        expected_and_present,
                    }),
                ));

                self.coordinator.signal_shutdown();
            }

            Ok(
                status @ (ValidationStatus::Cached
                | ValidationStatus::Valid { .. }
                | ValidationStatus::NoOutputs),
            ) => {
                self.target_planner
                    .update(&executable_target)
                    .await
                    .map_err(BuildWorkerError::TargetPlannerError)?;

                self.build_results
                    .add_computed_target(label.clone(), executable_target);

                if let ValidationStatus::Cached = status {
                    self.event_channel.send(Event::CacheHit(label.clone()));
                } else {
                    self.event_channel.send(Event::TargetBuilt(label.clone()));
                }

                self.build_queue.ack(&label);
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    async fn target(label: Label) -> ExecutableTarget {
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
            ExecutionResult::default(),
        )
        .await
        .unwrap()
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
        let s = Arc::new(Store::new(&w));
        let te = Arc::new(TargetExecutor::new(s.clone(), ec.clone()));
        let sres = Arc::new(SharedRuleExecutorState::new());
        BuildWorker::new(r, &w, l, bc, ec, bq, br, lr, te, s, sres).unwrap()
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
        br.add_computed_target(l.clone(), target(l).await);

        assert!(!help_worker.should_stop());
        assert!(main_worker.should_stop());
        assert!(help_worker.should_stop());
    }
}
