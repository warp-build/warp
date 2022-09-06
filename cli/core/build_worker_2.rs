use super::*;

pub struct BuildWorker {
    label_resolver: LabelResolver,
    target_planner: TargetPlanner,
    target_executor: TargetExecutor,
}

impl BuildWorker {
    #[tracing::instrument(name = "BuildWorker::execute", skip(self))]
    pub async fn execute(&mut self, label: &Label) -> Result<Label, BuildWorkerError> {
        let target = self.label_resolver.resolve(&label).await?;
        let executable_target = self.target_planner.plan(&target).await?;
        self.target_executor.schedule(&executable_target).await
    }
}

mod tests {}
