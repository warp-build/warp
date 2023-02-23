use super::shared_context::LocalSharedContext;
use super::*;
use crate::events::Event;
use thiserror::*;

#[cfg_attr(doc, aquamarine::aquamarine)]
/// A Local build execution worker.
///
/// The `LocalWorker` takes care of running one-task at a time out of the `TaskQueue` that lives in
/// the `LocalSharedContext`.
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
pub struct LocalWorker {
    role: Role,
    ctx: LocalSharedContext,
    env: ExecutionEnvironment,
    // target_planner: TargetPlanner,
}

#[async_trait]
impl Worker for LocalWorker {
    type Context = LocalSharedContext;

    fn new(role: Role, ctx: Self::Context) -> Result<Self, WorkerError> {
        let env = ExecutionEnvironment::new();

        /*
        let target_planner = TargetPlanner::new(
            ctx.task_results.clone(),
            ctx.artifact_store.clone(),
            ctx.source_manager.clone(),
            ctx.share_rule_executor_state.clone(),
            ctx.target_registry.clone(),
        )
        .map_err(LocalWorkerError::TargetPlannerError)?;
        */

        Ok(Self {
            role,
            ctx,
            env,
            // target_planner,
        })
    }

    #[tracing::instrument(name = "LocalWorker::setup_and_run", skip(self))]
    async fn setup_and_run(&mut self) -> Result<(), WorkerError> {
        loop {
            // NOTE(@ostera): we don't want things to burn CPU cycles
            tokio::time::sleep(std::time::Duration::from_micros(10)).await;
            let result = self.run().await;
            if result.is_err() {
                self.ctx.coordinator.signal_shutdown();
                self.finish();
                break result?;
            }
            if self.should_stop() {
                self.finish();
                break;
            }
        }
        Ok(())
    }
}

impl LocalWorker {
    pub fn should_stop(&self) -> bool {
        if Role::MainWorker == self.role && self.ctx.task_results.has_all_expected_targets() {
            self.ctx.coordinator.signal_shutdown();
        }
        self.ctx.coordinator.should_shutdown()
    }

    pub fn finish(&mut self) {
        if self.role == Role::MainWorker {
            self.ctx
                .event_channel
                .send(Event::BuildCompleted(std::time::Instant::now()))
        }
    }

    #[tracing::instrument(name = "LocalWorker::run", skip(self))]
    pub async fn run(&mut self) -> Result<(), LocalWorkerError> {
        let task = match self.ctx.task_queue.next() {
            Some(task) => task,
            None => return Ok(()),
        };

        dbg!(self.ctx.target_registry.get_target(task.target));

        let target = task.target;

        self.ctx.event_channel.send(Event::HandlingTarget {
            target: self.ctx.target_registry.get_target(target).to_string(),
            goal: task.goal.to_string(),
        });

        Ok(())
    }
}

#[derive(Error, Debug)]
pub enum LocalWorkerError {}

impl From<LocalWorkerError> for WorkerError {
    fn from(err: LocalWorkerError) -> Self {
        Self::LocalWorkerError(err)
    }
}

#[cfg(test)]
mod tests {
    #[tokio::test]
    async fn when_results_are_finished_main_worker_stops_then_helpers() {}
}
