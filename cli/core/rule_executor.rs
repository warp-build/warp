use super::*;

pub struct RuleExecutor {
    runtime: deno_core::JsRuntime,
}

pub struct ExecutionResult {
    actions: Vec<Action>,
    outs: FxHashSet<PathBuf>,
    srcs: FxHashSet<PathBuf>,
    run_script: Option<RunScript>,
}

impl RuleExecutor {
    pub async fn execute(
        &self,
        env: &ExecutionEnvironment,
        rule: &Rule,
        target: &Target,
    ) -> Result<ExecutionResult, RuleExecutorError> {
        // Make sure that the target has all the inputs needed to execute this rule.
        let config = rule.cfg.validate(&target.cfg)?;

        self.runtime
            .execute_script(
                &format!("<computed_target: {:?}>", &label.to_string()),
                &compute_program,
            )
            .map_err(|err| error::RuleExecError::ExecutionError {
                err,
                label: label.clone(),
                computed_target: Box::new(computed_target.clone()),
            })?;

        let actions = self
            .action_map
            .get(&label)
            .map(|entry| entry.value().clone())
            .unwrap_or_default();

        let outs: FxHashSet<PathBuf> = self
            .output_map
            .get(&label)
            .ok_or(error::RuleExecError::MissingDeclaredOutputs {
                label: label.clone(),
            })?
            .iter()
            .cloned()
            .collect();

        let run_script: Option<RunScript> = self.run_script_map.get(&label).map(|rs| rs.clone());

        let srcs: FxHashSet<PathBuf> = computed_target
            .target
            .config()
            .get_file_lists()
            .unwrap_or_default()
            .iter()
            .cloned()
            .collect();

        Ok(ExecutionResult {
            actions,
            outs,
            srcs,
            run_script,
        })
    }
}

mod tests {}
