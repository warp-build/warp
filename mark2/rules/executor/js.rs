use super::{net_module_loader::NetModuleLoader, RuleExecutor, RuleExecutorError};
use deno_core::Extension;
use futures::{Future, FutureExt};
use std::{pin::Pin, rc::Rc};
use thiserror::Error;

static JS_SNAPSHOT: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/JS_SNAPSHOT.bin"));

pub struct JsRuleExecutor {
    ctx: SharedJsContext,
    runtime: deno_core::JsRuntime,
    script_count: i32,
}

#[derive(Default, Debug, Clone)]
pub struct SharedJsContext {
    /*
    rule_map: Arc<DashMap<String, Rule>>,
    loaded_rules: FxHashMap<String, Rule>,
    loaded_modules: FxHashMap<String, ()>,
    action_map: Arc<DashMap<Label, Vec<Action>>>,
    output_map: Arc<DashMap<Label, Vec<PathBuf>>>,
    provides_map: Arc<DashMap<Label, FxHashMap<String, String>>>,
    env_map: Arc<DashMap<Label, FxHashMap<String, String>>>,
    run_script_map: Arc<DashMap<Label, RunScript>>,
    task_results: Arc<TaskResults>,
    */
}

impl RuleExecutor for JsRuleExecutor {
    type Context = SharedJsContext;

    fn new(ctx: Self::Context) -> Result<Self, RuleExecutorError>
    where
        Self: Sized,
    {
        let extension: deno_core::Extension = {
            /*
            let action_map = action_map.clone();
            let output_map = output_map.clone();
            let run_script_map = run_script_map.clone();

            let provides_map = ctx.provides_map.clone();
            let env_map = ctx.env_map.clone();
            let rule_map = ctx.rule_map.clone();

            let inner_state = InnerState {
                id: uuid::Uuid::new_v4(),
                action_map,
                output_map,
                provides_map,
                rule_map,
                run_script_map,
                env_map,
            };
            */

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
                    // state.put(inner_state.clone());
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
        let runtime = deno_core::JsRuntime::new(rt_options);

        let mut rule_executor = Self {
            ctx,
            runtime,
            script_count: 0,
        };

        // rule_executor.setup()?;

        Ok(rule_executor)
    }

    fn execute<'a>(
        &'a mut self,
        env: &crate::worker::ExecutionEnvironment,
        sig: &crate::model::Signature,
        deps: &crate::model::Dependencies,
    ) -> Pin<Box<dyn Future<Output = Result<(), RuleExecutorError>> + 'a>> {
        async move { todo!() }.boxed_local()
    }
}

#[derive(Error, Debug)]
pub enum JsRuleExecutorError {}
