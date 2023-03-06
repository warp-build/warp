use std::pin::Pin;

use super::{DefaultPlannerContext, Dependencies, Planner, PlannerError, PlanningFlow};
use crate::model::{ExecutableSpec, Signature};
use crate::rules::{RuleExecutor, SharedJsContext};
use crate::worker::ExecutionEnvironment;
use futures::{Future, FutureExt};

pub struct DefaultPlanner<RE: RuleExecutor> {
    ctx: DefaultPlannerContext,
    rule_executor: RE,
}

impl<RE> Planner for DefaultPlanner<RE>
where
    RE: RuleExecutor<Context = SharedJsContext>,
{
    type Context = DefaultPlannerContext;

    fn new(ctx: Self::Context) -> Result<Self, PlannerError> {
        Ok(Self {
            rule_executor: RE::new(ctx.into_js_ctx())?,
            ctx,
        })
    }

    fn plan<'a>(
        &'a mut self,
        sig: Signature,
        env: ExecutionEnvironment,
    ) -> Pin<Box<dyn Future<Output = Result<PlanningFlow, PlannerError>> + 'a>> {
        async move {
            let planning_start_time = chrono::Utc::now();

            let deps = match self.find_deps(&sig).await? {
                PlanningFlow::FoundAllDeps { deps } => deps,
                flow => return Ok(flow),
            };

            let plan = self.rule_executor.execute(&env, &sig, &deps).await?;

            let spec = ExecutableSpec::builder()
                .planning_start_time(planning_start_time)
                // .planning_end_time(plan.end_time)
                .deps(deps)
                .build()?;

            Ok(PlanningFlow::Planned { spec })
        }
        .boxed_local()
    }
}

impl<RE: RuleExecutor> DefaultPlanner<RE> {
    async fn find_deps(&self, sig: &Signature) -> Result<PlanningFlow, PlannerError> {
        Ok(PlanningFlow::FoundAllDeps {
            deps: Dependencies::default(),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
}
