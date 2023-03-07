use super::net_module_loader::NetModuleLoader;
use super::{FfiContext, RuleExecutor, RuleExecutorError, SharedJsContext};
use crate::model::rule::expander::Expander;
use crate::model::{Dependencies, Rule, RunScript, Signature};
use crate::rules::executor::compute_script::ComputeScript;
use crate::rules::ExecutionResult;
use crate::worker::ExecutionEnvironment;
use deno_core::Extension;
use futures::{Future, FutureExt};
use fxhash::{FxHashMap, FxHashSet};
use std::path::PathBuf;
use std::{pin::Pin, rc::Rc};
use tracing::trace;

static JS_SNAPSHOT: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/JS_SNAPSHOT.bin"));

pub struct JsRuleExecutor {
    ctx: SharedJsContext,
    js_runtime: deno_core::JsRuntime,
    script_count: i32,
}

impl RuleExecutor for JsRuleExecutor {
    type Context = SharedJsContext;

    fn new(ctx: Self::Context) -> Result<Self, RuleExecutorError>
    where
        Self: Sized,
    {
        let extension: deno_core::Extension = {
            let ffi_ctx: FfiContext = ctx.clone().into();
            Extension::builder("warp")
                .ops(vec![
                    /*
                    js_ffi::op_ctx_actions_copy::decl(),
                    js_ffi::op_ctx_actions_declare_outputs::decl(),
                    js_ffi::op_ctx_actions_declare_run_script::decl(),
                    js_ffi::op_ctx_actions_exec::decl(),
                    js_ffi::op_ctx_actions_run_shell::decl(),
                    js_ffi::op_ctx_actions_write_file::decl(),
                    js_ffi::op_ctx_declare_env::decl(),
                    js_ffi::op_ctx_declare_provides::decl(),
                    js_ffi::op_ctx_download::decl(),
                    js_ffi::op_ctx_extract::decl(),
                    js_ffi::op_ctx_fetch_provides::decl(),
                    js_ffi::op_ctx_set_permissions::decl(),
                    js_ffi::op_ctx_verify_checksum::decl(),
                    js_ffi::op_file_filename::decl(),
                    js_ffi::op_file_parent::decl(),
                    js_ffi::op_file_with_extension::decl(),
                    js_ffi::op_label_name::decl(),
                    js_ffi::op_label_path::decl(),
                    js_ffi::op_rule_new::decl(),
                    */
                ])
                .state(move |state| {
                    state.put(ffi_ctx.clone());
                    Ok(())
                })
                .build()
        };

        let rt_options = deno_core::RuntimeOptions {
            startup_snapshot: Some(deno_core::Snapshot::Static(JS_SNAPSHOT)),
            module_loader: Some(Rc::new(NetModuleLoader {})),
            // v8_platform: Some(v8::Platform::new_single_threaded(true).make_shared()),
            extensions: vec![extension, deno_console::init()],
            ..Default::default()
        };
        let mut js_runtime = deno_core::JsRuntime::new(rt_options);

        js_runtime
            .execute_script("<prelude>", include_str!("prelude.js"))
            .map_err(RuleExecutorError::DenoExecutionError)?;

        Ok(Self {
            ctx,
            js_runtime,
            script_count: 0,
        })
    }

    fn execute<'a>(
        &'a mut self,
        env: &'a ExecutionEnvironment,
        sig: &'a Signature,
        deps: &'a Dependencies,
    ) -> Pin<Box<dyn Future<Output = Result<ExecutionResult, RuleExecutorError>> + 'a>> {
        async move {
            let rule: Rule = todo!(); // self.ctx.rule_store.get(sig.rule()).await?;

            self.script_count += 1;
            trace!("executing script {}", self.script_count);

            let config = Expander
                .expand(&rule, &sig)
                .await
                .map_err(RuleExecutorError::ConfigExpanderError)?;

            let compute_program = ComputeScript::as_js_source(
                self.ctx.task_results.clone(),
                env,
                sig,
                deps,
                &rule,
                &config,
            );

            trace!("Executing: {}", compute_program);

            let script_name = format!("<sig: {:?}>", &sig.target().to_string());

            self.js_runtime
                .execute_script(&script_name, &compute_program)
                .map_err(|err| RuleExecutorError::ExecutionError {
                    err,
                    target: (*sig.target().original_target()).clone(),
                    sig: Box::new(sig.clone()),
                    rule: rule.clone(),
                })?;

            trace!("Done!");

            let actions = self
                .ctx
                .action_map
                .get(&sig.target().target_id())
                .map(|entry| entry.value().clone())
                .unwrap_or_default();

            let outs: FxHashSet<PathBuf> = self
                .ctx
                .output_map
                .get(&sig.target().target_id())
                .ok_or(RuleExecutorError::MissingDeclaredOutputs {
                    target: sig.target().target_id(),
                    outputs: (*self.ctx.output_map).clone(),
                })?
                .iter()
                .cloned()
                .collect();

            let run_script: Option<RunScript> = self
                .ctx
                .run_script_map
                .get(&sig.target().target_id())
                .map(|rs| rs.clone());

            let srcs = config.get_file_set();

            let provides = self
                .ctx
                .provides_map
                .get(&sig.target().target_id())
                .map(|r| r.value().clone())
                .unwrap_or_default()
                .into_iter()
                .map(|(k, v)| (k, PathBuf::from(v)))
                .collect::<FxHashMap<String, PathBuf>>();

            let env = self
                .ctx
                .env_map
                .get(&sig.target().target_id())
                .map(|r| r.value().clone())
                .unwrap_or_default()
                .into_iter()
                .collect::<FxHashMap<String, String>>();

            self.ctx.action_map.clear();
            self.ctx.output_map.clear();

            Ok(ExecutionResult {
                actions,
                outs,
                srcs,
                run_script,
                provides,
                env,
            })
        }
        .boxed_local()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
}
