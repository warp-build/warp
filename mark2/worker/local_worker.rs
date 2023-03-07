use core::future::Future;
use std::pin::Pin;

use super::*;
use crate::{
    planner::{DefaultPlannerContext, Planner, PlanningFlow},
    resolver::{ResolutionFlow, Resolver, ResolverError},
};
use futures::FutureExt;
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
pub struct LocalWorker<R: Resolver, P: Planner> {
    role: Role,
    ctx: LocalSharedContext<R>,
    planner: P,
    env: ExecutionEnvironment,
}

pub enum WorkerFlow {
    TaskCompleted(Task),
    RetryLater,
}

impl<R, P, PCtx> Worker for LocalWorker<R, P>
where
    R: Resolver,
    P: Planner<Context = PCtx>,
    PCtx: From<LocalSharedContext<R>>,
{
    type Context = LocalSharedContext<R>;

    fn new(role: Role, ctx: Self::Context) -> Result<Self, WorkerError> {
        let env = ExecutionEnvironment::new();
        let planner = P::new(ctx.clone().into())?;
        Ok(Self {
            role,
            ctx,
            env,
            planner,
        })
    }

    fn run<'a>(&'a mut self) -> Pin<Box<dyn Future<Output = Result<(), WorkerError>> + 'a>> {
        async move {
            for task in self.role.tasks() {
                let _ = self.ctx.task_queue.queue(*task)?;
            }
            if self.role.is_main_worker() {
                self.ctx.task_results.mark_as_ready();
            }

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
        .boxed_local()
    }
}

impl<R: Resolver, P: Planner> LocalWorker<R, P> {
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

    pub async fn handle_task(&mut self, task: Task) -> Result<WorkerFlow, LocalWorkerError> {
        let target = self.ctx.target_registry.get_target(task.target);

        let signature = match self.ctx.resolver.resolve(task.goal, target).await? {
            ResolutionFlow::Resolved { signature } => signature,
            _flow => return Ok(WorkerFlow::RetryLater),
        };

        let _executable_spec = match self.planner.plan(signature, self.env.clone()).await? {
            PlanningFlow::Planned { spec } => spec,
            _flow => return Ok(WorkerFlow::RetryLater),
        };

        /*
        let artifact_manifest = match self.executor.execute(executable_spec)? {
            WorkerFlow::Executed(manifest) => artifact_manifest,
            _flow => return Ok(WorkerFlow::RetryLater),
        };

        self.ctx.store.save(manifest).await?;

        self.ctx.task_results.add(task.target, manifest, executable_spec);
        */
        Ok(WorkerFlow::TaskCompleted(task))
    }
}

#[derive(Error, Debug)]
pub enum LocalWorkerError {
    #[error(transparent)]
    ResolverError(ResolverError),

    #[error(transparent)]
    PlannerError(PlannerError),
}

impl From<PlannerError> for WorkerError {
    fn from(value: PlannerError) -> Self {
        WorkerError::PlannerError(value)
    }
}

impl From<TaskQueueError> for WorkerError {
    fn from(err: TaskQueueError) -> Self {
        WorkerError::TaskQueueError(err)
    }
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

impl From<PlannerError> for LocalWorkerError {
    fn from(value: PlannerError) -> Self {
        LocalWorkerError::PlannerError(value)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::events::EventChannel;
    use crate::model::{ConcreteTarget, Goal, Signature, Target};
    use crate::planner::PlannerError;
    use crate::{sync::*, Config};
    use async_trait::async_trait;
    use quickcheck::Arbitrary;

    #[derive(Clone)]
    struct NoopPlanner;
    #[derive(Debug, Clone)]
    struct NoopContext;

    impl<R: Resolver> From<LocalSharedContext<R>> for NoopContext {
        fn from(_value: LocalSharedContext<R>) -> Self {
            Self
        }
    }

    impl Planner for NoopPlanner {
        type Context = NoopContext;

        fn new(_ctx: Self::Context) -> Result<Self, PlannerError> {
            Ok(Self)
        }

        fn plan(
            &mut self,
            sig: Signature,
            env: ExecutionEnvironment,
        ) -> Pin<Box<dyn Future<Output = Result<PlanningFlow, PlannerError>>>> {
            async move {
                Ok(PlanningFlow::MissingDeps {
                    requirements: vec![],
                })
            }
            .boxed_local()
        }
    }

    #[derive(Clone)]
    struct NoopResolver;

    #[async_trait]
    impl Resolver for NoopResolver {
        async fn resolve(
            &self,
            _goal: Goal,
            _target: Arc<Target>,
        ) -> Result<ResolutionFlow, ResolverError> {
            Ok(ResolutionFlow::IncompatibleTarget)
        }
    }

    #[tokio::test]
    async fn when_coordinator_marks_shutdown_the_worker_stops() {
        let ec = Arc::new(EventChannel::new());
        let config = Config::builder().build().unwrap();
        let ctx = LocalSharedContext::new(ec, config, NoopResolver);

        ctx.coordinator.signal_shutdown();

        let mut w: LocalWorker<NoopResolver, NoopPlanner> =
            LocalWorker::new(Role::MainWorker(vec![]), ctx).unwrap();
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

        let mut w: LocalWorker<ErrResolver, NoopPlanner> =
            LocalWorker::new(Role::MainWorker(vec![]), ctx.clone()).unwrap();
        let err = w.run().await.unwrap_err();

        assert_matches!(
            err,
            WorkerError::LocalWorkerError(LocalWorkerError::ResolverError(ResolverError::Unknown(
                _
            )))
        );
        assert!(ctx.coordinator.should_shutdown());
    }

    #[tokio::test]
    async fn on_planner_error_worker_signal_shutdown() {
        #[derive(Clone)]
        struct ErrPlanner;
        impl Planner for ErrPlanner {
            type Context = ();

            fn new(_ctx: Self::Context) -> Result<Self, PlannerError> {
                Ok(Self)
            }

            fn plan<'a>(
                &'a mut self,
                _sig: Signature,
                _env: ExecutionEnvironment,
            ) -> Pin<Box<dyn Future<Output = Result<PlanningFlow, PlannerError>> + 'a>>
            {
                async move { Err(PlannerError::Unknown) }.boxed_local()
            }
        }

        #[derive(Clone)]
        struct DummyResolver;

        #[async_trait]
        impl Resolver for DummyResolver {
            async fn resolve(
                &self,
                goal: Goal,
                target: Arc<Target>,
            ) -> Result<ResolutionFlow, ResolverError> {
                let target = ConcreteTarget::new(goal, target, "".into());
                let signature = Signature::builder()
                    .rule("dummy_rule".into())
                    .target(target)
                    .build()
                    .unwrap();
                Ok(ResolutionFlow::Resolved { signature })
            }
        }

        let ec = Arc::new(EventChannel::new());
        let config = Config::builder().build().unwrap();
        let ctx = LocalSharedContext::new(ec, config, DummyResolver);

        let mut gen = quickcheck::Gen::new(100);
        let target: Target = Arbitrary::arbitrary(&mut gen);
        let goal: Goal = Arbitrary::arbitrary(&mut gen);
        let target_id = ctx.target_registry.register_target(target);
        let task = Task::new(goal, target_id);
        ctx.task_queue.queue(task).unwrap();
        assert!(!ctx.task_queue.is_empty());

        let mut w: LocalWorker<DummyResolver, ErrPlanner> =
            LocalWorker::new(Role::MainWorker(vec![]), ctx.clone()).unwrap();
        let err = w.run().await.unwrap_err();

        assert_matches!(
            err,
            WorkerError::LocalWorkerError(LocalWorkerError::PlannerError(PlannerError::Unknown))
        );
        assert!(ctx.coordinator.should_shutdown());
    }
}
