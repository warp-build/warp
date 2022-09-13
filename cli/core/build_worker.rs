use super::Event;
use super::*;
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
        target: Label,
        coordinator: Arc<BuildCoordinator>,
        event_channel: Arc<EventChannel>,
        build_queue: Arc<BuildQueue>,
        build_results: Arc<BuildResults>,
        label_resolver: Arc<LabelResolver>,
        target_executor: Arc<TargetExecutor>,
    ) -> Self {
        let env = ExecutionEnvironment::new();
        let target_planner = TargetPlanner::new(build_results.clone());
        Self {
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
        }
    }

    #[tracing::instrument(name = "BuildWorker::run", skip(self))]
    pub async fn setup_and_run(&mut self, max_concurrency: usize) -> Result<(), BuildWorkerError> {
        let result = {
            self.setup(max_concurrency).await?;
            loop {
                // NOTE(@ostera): we don't want things to burn CPU cycles
                tokio::time::sleep(std::time::Duration::from_micros(100)).await;
                self.run().await?;
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

    pub async fn run(&mut self) -> Result<(), BuildWorkerError> {
        if let Some(label) = self.build_queue.next() {
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

            if let Err(err) = self.target_executor.execute(&executable_target).await {
                self.event_channel.send(Event::BuildError(
                    label.clone(),
                    BuildError::TargetExecutorError(err),
                ));
                self.coordinator.signal_shutdown();
            } else {
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
        let lr = Arc::new(LabelResolver::new());
        let s = Arc::new(Store::new(&w));
        let te = Arc::new(TargetExecutor::new(s));
        BuildWorker::new(r, l, bc, ec, bq, br, lr, te)
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
