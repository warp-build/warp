use super::*;
use crate::resolver::{ResolutionFlow, Resolver, ResolverError, Signature};
use thiserror::*;

#[cfg_attr(doc, aquamarine::aquamarine)]
/// A Local build execution worker.
///
/// The `LocalWorker` takes care of running one-task at a time out of the `TaskQueue` that lives in
/// the `LocalSharedContext`. It contains a series of strategies for resolving, planning,a dn
/// executing work.
///
/// It orchestrates its lifecycle by checking with the `Coordinator` (also in the `LocalSharedContext`).
///
/// It's flow goes:
///
/// ```mermaid
/// graph TD
///   Idle -->|run| loop
///   loop -->|has next task| execute
///
///   execute
///   --> resolve_target
///   --> plan_signature
///   --> execute_plan
///   --> cache_results
///   --> should_stop
///
///   loop -->|no next task| should_stop
///   should_stop -->|is main && has all results| signal_shutdown
///   should_stop -->|no| sleep
///   sleep -->|timeout| loop
/// ```
///
#[derive(Debug)]
pub struct LocalWorker<R: Resolver> {
    role: Role,
    ctx: LocalSharedContext<R>,
    env: ExecutionEnvironment,
}

pub enum WorkerFlow {
    TaskCompleted(Task),
    RetryLater,
}

#[async_trait]
impl<R: Resolver> Worker for LocalWorker<R> {
    type Context = LocalSharedContext<R>;

    fn new(role: Role, ctx: Self::Context) -> Result<Self, WorkerError> {
        let env = ExecutionEnvironment::new();
        Ok(Self { role, ctx, env })
    }

    #[tracing::instrument(name = "LocalWorker::run", skip(self))]
    async fn run(&mut self) -> Result<(), WorkerError> {
        while self.ctx.coordinator.should_run() {
            // NOTE(@ostera): we don't want things to burn CPU cycles
            tokio::time::sleep(std::time::Duration::from_micros(10)).await;
            if let Err(err) = self.poll().await {
                self.ctx.coordinator.signal_shutdown();
                return Err(WorkerError::LocalWorkerError(err));
            }

            if self.role.is_main_worker() && self.ctx.task_results.has_all_expected_targets() {
                self.ctx.coordinator.signal_shutdown();
                break;
            }
        }
        Ok(())
    }
}

impl<R: Resolver> LocalWorker<R> {
    pub async fn poll(&mut self) -> Result<(), LocalWorkerError> {
        let task = match self.ctx.task_queue.next() {
            Some(task) => task,
            None => return Ok(()),
        };

        match self.handle_task(task).await? {
            WorkerFlow::TaskCompleted(task) => {
                self.ctx.task_queue.ack(task);
                Ok(())
            }
            WorkerFlow::RetryLater => {
                self.ctx.task_queue.nack(task);
                Ok(())
            }
        }
    }

    pub async fn handle_task(&self, task: Task) -> Result<WorkerFlow, LocalWorkerError> {
        let target = self.ctx.target_registry.get_target(task.target);

        let _signature = match self.ctx.resolver.resolve(task.goal, target).await? {
            ResolutionFlow::Resolved { signature } => signature,
            _flow => return Ok(WorkerFlow::RetryLater),
        };

        /*
        let executable_spec = match self.planner.plan(task, signature, &self.env)? {
            WorkerFlow::Planned(spec) => spec,
            flow => return Ok(flow)
        };

        let manifest = match self.executor.execute(executable_spec)? {
            WorkerFlow::Executed(manifest) => manifest,
            flow => return Ok(flow)
        };

        self.ctx.store.save(manifest).await?;

        self.ctx.task_results.add(task.target, manifest, executable_spec);
        */

        self.ctx.task_queue.ack(task);

        Ok(WorkerFlow::TaskCompleted(task))
    }
}

#[derive(Error, Debug)]
pub enum LocalWorkerError {
    #[error(transparent)]
    ResolverError(ResolverError),
}

impl From<LocalWorkerError> for WorkerError {
    fn from(err: LocalWorkerError) -> Self {
        Self::LocalWorkerError(err)
    }
}

impl From<ResolverError> for LocalWorkerError {
    fn from(err: ResolverError) -> Self {
        Self::ResolverError(err)
    }
}

#[cfg(test)]
mod tests {
    use quickcheck::Arbitrary;

    use super::*;
    use crate::events::EventChannel;
    use crate::resolver::{Goal, Target, TargetId};
    use crate::{sync::*, Config};

    #[derive(Clone)]
    struct NoopResolver;

    #[async_trait]
    impl Resolver for NoopResolver {
        async fn resolve(
            &self,
            _goal: Goal,
            _target: Arc<Target>,
        ) -> Result<ResolutionFlow, ResolverError> {
            Ok(ResolutionFlow::MissingDependencies)
        }
    }

    #[tokio::test]
    async fn when_coordinator_marks_shutdown_the_worker_stops() {
        let ec = Arc::new(EventChannel::new());
        let config = Config::builder().build().unwrap();
        let ctx = LocalSharedContext::new(ec, config, NoopResolver);

        ctx.coordinator.signal_shutdown();

        let mut w = LocalWorker::new(Role::MainWorker, ctx).unwrap();
        w.run().await.unwrap();
    }

    #[tokio::test]
    async fn on_resolver_error_worker_signal_shutdown() {
        #[derive(Clone)]
        struct ErrResolver;

        #[async_trait]
        impl Resolver for ErrResolver {
            async fn resolve(
                &self,
                _goal: Goal,
                _target: Arc<Target>,
            ) -> Result<ResolutionFlow, ResolverError> {
                Err(ResolverError::Unknown("test error".to_string()))
            }
        }

        let ec = Arc::new(EventChannel::new());
        let config = Config::builder().build().unwrap();
        let ctx = LocalSharedContext::new(ec, config, ErrResolver);

        let mut gen = quickcheck::Gen::new(100);
        let target: Target = Arbitrary::arbitrary(&mut gen);
        let goal: Goal = Arbitrary::arbitrary(&mut gen);
        let target_id = ctx.target_registry.register_target(target);
        let task = Task::new(goal, target_id);
        ctx.task_queue.queue(task).unwrap();
        assert!(!ctx.task_queue.is_empty());

        let mut w = LocalWorker::new(Role::MainWorker, ctx.clone()).unwrap();
        let result = w.run().await;

        assert!(result.is_err());
        assert!(ctx.coordinator.should_shutdown());
    }
}
