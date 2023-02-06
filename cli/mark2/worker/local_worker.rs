use super::shared_context::SharedContext;
use super::*;
use crate::events::Event;
use crate::resolver::*;
use thiserror::*;

#[derive(Error, Debug)]
pub enum LocalWorkerError {}

pub struct LocalWorker {
    role: Role,
    ctx: SharedContext,
    env: ExecutionEnvironment,
    // target_planner: TargetPlanner,
}

impl LocalWorker {
    pub fn new(role: Role, ctx: SharedContext) -> Result<Self, LocalWorkerError> {
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
    pub async fn setup_and_run(&mut self) -> Result<(), LocalWorkerError> {
        loop {
            // NOTE(@ostera): we don't want things to burn CPU cycles
            tokio::time::sleep(std::time::Duration::from_micros(10)).await;
            let result = self.run().await;
            if result.is_err() {
                self.ctx.coordinator.signal_shutdown();
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

        let target = task.target;

        self.ctx.event_channel.send(Event::HandlingTarget {
            target: self.ctx.target_registry.get_target(target).to_string(),
            goal: task.goal.to_string(),
        });

        Ok(())
    }

    #[inline]
    async fn requeue(&self, task: Task, deps: &[TargetId]) -> Result<(), LocalWorkerError> {
        /*
        if let Err(QueueError::DependencyCycle(err)) = self.build_queue.queue_deps(task, deps) {
            self.ctx.event_channel.send(Event::BuildError {
                target: (*self.ctx.target_registry.get_target(task.target)).to_owned(),
                error: BuildError::BuildResultError(err),
            });
            self.ctx.coordinator.signal_shutdown();
        }

        self.build_queue.nack(task);
        */

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    #[tokio::test]
    async fn when_results_are_finished_main_worker_stops_then_helpers() {}
}
