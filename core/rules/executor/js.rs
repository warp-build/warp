use super::net_module_loader::NetModuleLoader;
use super::{js_ffi, FfiContext, RuleExecutor, RuleExecutorError, SharedJsContext};
use crate::model::rule::expander::Expander;
use crate::model::{Dependencies, ExecutionEnvironment, Rule, RunScript, Signature};
use crate::rules::executor::compute_script::ComputeScript;
use crate::rules::ExecutionResult;
use async_trait::async_trait;
use deno_core::Extension;
use fxhash::{FxHashMap, FxHashSet};
use std::collections::BTreeMap;
use std::path::PathBuf;
use std::rc::Rc;
use tokio::fs;
use tracing::trace;

static JS_SNAPSHOT: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/JS_SNAPSHOT.bin"));

pub struct JsRuleExecutor {
    ctx: SharedJsContext,
    js_runtime: deno_core::JsRuntime,
}

#[async_trait(?Send)]
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
                    js_ffi::op_target_name::decl(),
                    js_ffi::op_target_path::decl(),
                    js_ffi::op_target_parent_path::decl(),
                    js_ffi::op_rule_new::decl(),
                ])
                .state(move |state| {
                    state.put(ffi_ctx.clone());
                    Ok(())
                })
                .build()
        };

        let rt_options = deno_core::RuntimeOptions {
            startup_snapshot: Some(deno_core::Snapshot::Static(JS_SNAPSHOT)),
            module_loader: Some(Rc::new(NetModuleLoader {
                rule_store: ctx.rule_store.clone(),
            })),
            // v8_platform: Some(v8::Platform::new_single_threaded(true).make_shared()),
            extensions: vec![extension, deno_console::init()],
            ..Default::default()
        };
        let mut js_runtime = deno_core::JsRuntime::new(rt_options);

        js_runtime
            .execute_script("<prelude>", include_str!("prelude.js"))
            .map_err(RuleExecutorError::DenoExecutionError)?;

        Ok(Self { ctx, js_runtime })
    }

    async fn execute(
        &mut self,
        env: &ExecutionEnvironment,
        sig: &Signature,
        deps: &Dependencies,
    ) -> Result<ExecutionResult, RuleExecutorError> {
        let rule = self.load_rule(sig.rule()).await?;

        let config = Expander
            .expand(&rule, sig)
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
            .collect::<BTreeMap<String, PathBuf>>();

        let shell_env = self
            .ctx
            .shell_env_map
            .get(&sig.target().target_id())
            .map(|r| r.value().clone())
            .unwrap_or_default()
            .into_iter()
            .collect::<FxHashMap<String, String>>();

        let toolchains = rule.toolchains().to_vec();

        self.ctx.action_map.clear();
        self.ctx.output_map.clear();

        Ok(ExecutionResult {
            actions,
            outs,
            srcs,
            toolchains,
            run_script,
            provides,
            shell_env,
        })
    }
}

impl JsRuleExecutor {
    #[tracing::instrument(name = "JsRuleExecutor::load_rule", skip(self))]
    async fn load_rule(&mut self, rule_name: &str) -> Result<Rule, RuleExecutorError> {
        // NOTE(@ostera): avoid IO and redefining rules if it is already computed in this worker
        if let Some(r) = self.ctx.loaded_rules.get(rule_name) {
            return Ok(r.clone());
        }

        let rule_file = self.ctx.rule_store.get(rule_name).await?;

        self.load_file(rule_file).await?;

        let rule_normalized_name = self.ctx.rule_store.normalize_name(rule_name);

        let rule = self
            .ctx
            .rule_map
            .get(&rule_normalized_name)
            .map(|r| r.value().clone())
            .ok_or_else(|| RuleExecutorError::RuleNotFound {
                rule_name: rule_name.to_string(),
            })?;

        self.ctx
            .loaded_rules
            .insert(rule_name.to_string(), rule.clone());

        Ok(rule)
    }

    #[tracing::instrument(name = "JsRuleExecutor::load_file", skip(self), ret)]
    async fn load_file(&mut self, file: PathBuf) -> Result<(), RuleExecutorError> {
        let module_name = format!("file://{}", file.to_str().unwrap());
        let module_code =
            fs::read_to_string(&file)
                .await
                .map_err(|err| RuleExecutorError::CouldNotReadFile {
                    file: file.clone(),
                    err,
                })?;
        self.load(&module_name, Some(module_code)).await
    }

    #[tracing::instrument(name = "JsRuleExecutor::load", skip(self, module_code))]
    async fn load(
        &mut self,
        module_name: &str,
        module_code: Option<String>,
    ) -> Result<(), RuleExecutorError> {
        if self
            .ctx
            .loaded_modules
            .contains_key(&module_name.to_string())
        {
            return Ok(());
        }

        let mod_specifier =
            url::Url::parse(module_name).map_err(|reason| RuleExecutorError::BadModuleName {
                module_name: module_name.to_string(),
                reason,
            })?;

        let mod_id = self
            .js_runtime
            .load_side_module(&mod_specifier, module_code)
            .await
            .map_err(|reason| RuleExecutorError::ModuleResolutionError {
                module_name: module_name.to_string(),
                reason,
            })?;

        let eval_future = self.js_runtime.mod_evaluate(mod_id);

        self.js_runtime
            .run_event_loop(false)
            .await
            .map_err(|reason| RuleExecutorError::ModuleEvaluationError {
                module_name: module_name.to_string(),
                reason,
            })?;

        let _ = eval_future.await.unwrap();

        self.js_runtime
            .get_module_namespace(mod_id)
            .map_err(|reason| RuleExecutorError::ModuleEvaluationError {
                module_name: module_name.to_string(),
                reason,
            })?;

        self.ctx.loaded_modules.insert(module_name.to_string(), ());

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::rule::{self, Value};
    use crate::model::ConcreteTarget;
    use crate::resolver::TargetRegistry;
    use crate::rules::RuleStore;
    use crate::sync::Arc;
    use crate::worker::TaskResults;
    use crate::{Config, Goal};
    use assert_fs::prelude::*;
    use url::Url;

    #[tokio::test]
    async fn executes_a_rule_and_collects_results() {
        // 1. Set up the directories and side-effects

        let rule_store_root = assert_fs::TempDir::new().unwrap();
        // let rule_store_root = rule_store_root.into_persistent();
        dbg!(&rule_store_root.path());

        let invocation_dir = assert_fs::TempDir::new().unwrap();
        // let invocation_dir = invocation_dir.into_persistent();
        dbg!(&invocation_dir.path());

        let test_file = invocation_dir.child("file.ex");
        test_file.touch().unwrap();

        // NOTE(@ostera): This mock will be used to download a test_rule as if it was from the
        // internet.
        let m = mockito::mock("GET", "/test_rule.js")
            .with_status(200)
            .with_body(
                include_str!("./fixtures/test_rule.js")
                    .replace("{URL}", &mockito::server_url())
                    .as_bytes(),
            )
            .create();

        // 2. Configure Warp and create all the dependencies to the RuleExecutor

        let config = Config::builder()
            .rule_store_root(rule_store_root.path().into())
            .public_rule_store_url(mockito::server_url().parse::<Url>().unwrap())
            .invocation_dir(invocation_dir.path().into())
            .build()
            .unwrap();

        let rule_store = RuleStore::new(&config).into();
        let target_registry: Arc<TargetRegistry> = TargetRegistry::default().into();
        let task_results = TaskResults::new(target_registry.clone()).into();
        let ctx = SharedJsContext::new(target_registry.clone(), task_results, rule_store);

        // 3. Create our planning inputs

        let goal = Goal::Build;
        let target = Arc::new("./my/file.ex".into());
        let target_id = target_registry.register_target(&target);
        let c_target = ConcreteTarget::new(goal, target_id, target.clone(), "".into());

        let mut config = rule::Config::default();
        config.insert("name".into(), Value::Target((*target).clone()));
        config.insert(
            "srcs".into(),
            Value::List(vec![Value::File("./my/file.ex".into())]),
        );

        let env = Default::default();
        let sig = Signature::builder()
            .rule("test_rule".into())
            .target(c_target)
            .config(config)
            .build()
            .unwrap();

        let deps = Dependencies::default();

        // 4. Create the executor and run!

        let mut exec = JsRuleExecutor::new(ctx).unwrap();
        let res = exec.execute(&env, &sig, &deps).await.unwrap();

        assert!(!res.outs.is_empty());
        assert_eq!(
            res.outs.into_iter().next().unwrap(),
            PathBuf::from("./my/file.ex")
        );

        m.assert();
    }

    #[tokio::test]
    async fn fails_to_compute_rule_without_outputs() {
        // 1. Set up the directories and side-effects

        let rule_store_root = assert_fs::TempDir::new().unwrap();
        // let rule_store_root = rule_store_root.into_persistent();
        dbg!(&rule_store_root.path());

        let invocation_dir = assert_fs::TempDir::new().unwrap();
        // let invocation_dir = invocation_dir.into_persistent();
        dbg!(&invocation_dir.path());

        let test_file = invocation_dir.child("file.ex");
        test_file.touch().unwrap();

        // NOTE(@ostera): This mock will be used to download a test_rule as if it was from the
        // internet.
        let m = mockito::mock("GET", "/test_rule.js")
            .with_status(200)
            .with_body(
                include_str!("./fixtures/missing_declared_outputs.js")
                    .replace("{URL}", &mockito::server_url())
                    .as_bytes(),
            )
            .create();

        // 2. Configure Warp and create all the dependencies to the RuleExecutor

        let config = Config::builder()
            .rule_store_root(rule_store_root.path().into())
            .public_rule_store_url(mockito::server_url().parse::<Url>().unwrap())
            .invocation_dir(invocation_dir.path().into())
            .build()
            .unwrap();

        let rule_store = RuleStore::new(&config).into();
        let target_registry: Arc<TargetRegistry> = TargetRegistry::default().into();
        let task_results = TaskResults::new(target_registry.clone()).into();
        let ctx = SharedJsContext::new(target_registry.clone(), task_results, rule_store);

        // 3. Create our planning inputs

        let goal = Goal::Build;
        let target = Arc::new("./my/file.ex".into());
        let target_id = target_registry.register_target(&target);
        let c_target = ConcreteTarget::new(goal, target_id, target.clone(), "".into());

        let mut config = rule::Config::default();
        config.insert("name".into(), Value::Target((*target).clone()));

        let env = Default::default();
        let sig = Signature::builder()
            .rule("test_rule".into())
            .target(c_target)
            .config(config)
            .build()
            .unwrap();

        let deps = Dependencies::default();

        // 4. Create the executor and run!

        let mut exec = JsRuleExecutor::new(ctx).unwrap();
        let err = exec.execute(&env, &sig, &deps).await.unwrap_err();

        assert_matches!(
            err,
            RuleExecutorError::MissingDeclaredOutputs { target, .. } if target == target_id
        );

        m.assert();
    }

    #[tokio::test]
    async fn fails_if_the_rule_errors() {
        // 1. Set up the directories and side-effects

        let rule_store_root = assert_fs::TempDir::new().unwrap();
        // let rule_store_root = rule_store_root.into_persistent();
        dbg!(&rule_store_root.path());

        let invocation_dir = assert_fs::TempDir::new().unwrap();
        // let invocation_dir = invocation_dir.into_persistent();
        dbg!(&invocation_dir.path());

        let test_file = invocation_dir.child("file.ex");
        test_file.touch().unwrap();

        // NOTE(@ostera): This mock will be used to download a test_rule as if it was from the
        // internet.
        let m = mockito::mock("GET", "/test_rule.js")
            .with_status(200)
            .with_body(
                include_str!("./fixtures/rule_with_errors.js")
                    .replace("{URL}", &mockito::server_url())
                    .as_bytes(),
            )
            .create();

        // 2. Configure Warp and create all the dependencies to the RuleExecutor

        let config = Config::builder()
            .rule_store_root(rule_store_root.path().into())
            .public_rule_store_url(mockito::server_url().parse::<Url>().unwrap())
            .invocation_dir(invocation_dir.path().into())
            .build()
            .unwrap();

        let rule_store = RuleStore::new(&config).into();
        let target_registry: Arc<TargetRegistry> = TargetRegistry::default().into();
        let task_results = TaskResults::new(target_registry.clone()).into();
        let ctx = SharedJsContext::new(target_registry.clone(), task_results, rule_store);

        // 3. Create our planning inputs

        let goal = Goal::Build;
        let target = Arc::new("./my/file.ex".into());
        let target_id = target_registry.register_target(&target);
        let c_target = ConcreteTarget::new(goal, target_id, target.clone(), "".into());

        let mut config = rule::Config::default();
        config.insert("name".into(), Value::Target((*target).clone()));

        let env = Default::default();
        let sig = Signature::builder()
            .rule("test_rule".into())
            .target(c_target)
            .config(config)
            .build()
            .unwrap();

        let deps = Dependencies::default();

        // 4. Create the executor and run!

        let mut exec = JsRuleExecutor::new(ctx).unwrap();
        let err = exec.execute(&env, &sig, &deps).await.unwrap_err();

        assert_matches!(
            err,
            RuleExecutorError::ExecutionError { target, .. } if target == target
        );

        m.assert();
    }

    #[tokio::test]
    async fn rules_can_import_other_rules() {
        // 1. Set up the directories and side-effects

        let rule_store_root = assert_fs::TempDir::new().unwrap();
        // let rule_store_root = rule_store_root.into_persistent();
        dbg!(&rule_store_root.path());

        let invocation_dir = assert_fs::TempDir::new().unwrap();
        // let invocation_dir = invocation_dir.into_persistent();
        dbg!(&invocation_dir.path());

        let test_file = invocation_dir.child("file.ex");
        test_file.touch().unwrap();

        let mock_url = mockito::server_url().parse::<Url>().unwrap();

        // NOTE(@ostera): This mock will be used to download a test_rule as if it was from the
        // internet.
        let _m1 = mockito::mock("GET", "/rule_with_dep.js")
            .with_status(200)
            .with_body(
                include_str!("./fixtures/rule_with_dep.js")
                    .replace("{URL}", &mockito::server_url())
                    .as_bytes(),
            )
            .create();

        let _m2 = mockito::mock("GET", "/dep_rule.js")
            .with_status(200)
            .with_body(
                include_str!("./fixtures/dep_rule.js")
                    .replace("{URL}", &mockito::server_url())
                    .as_bytes(),
            )
            .create();

        // 2. Configure Warp and create all the dependencies to the RuleExecutor

        let config = Config::builder()
            .rule_store_root(rule_store_root.path().into())
            .public_rule_store_url(mock_url)
            .invocation_dir(invocation_dir.path().into())
            .build()
            .unwrap();

        let rule_store = RuleStore::new(&config).into();
        let target_registry: Arc<TargetRegistry> = TargetRegistry::default().into();
        let task_results = TaskResults::new(target_registry.clone()).into();
        let ctx = SharedJsContext::new(target_registry.clone(), task_results, rule_store);

        // 3. Create our planning inputs

        let goal = Goal::Build;
        let target = Arc::new("./my/file.ex".into());
        let target_id = target_registry.register_target(&target);
        let c_target = ConcreteTarget::new(goal, target_id, target.clone(), "".into());

        let mut config = rule::Config::default();
        config.insert("name".into(), Value::Target((*target).clone()));
        config.insert(
            "srcs".into(),
            Value::List(vec![Value::File("./my/file.ex".into())]),
        );

        let env = Default::default();
        let sig = Signature::builder()
            .rule("rule_with_dep".into())
            .target(c_target)
            .config(config)
            .build()
            .unwrap();

        let deps = Dependencies::default();

        // 4. Create the executor and run!

        let mut exec = JsRuleExecutor::new(ctx).unwrap();
        let res = exec.execute(&env, &sig, &deps).await.unwrap();

        assert!(!res.outs.is_empty());
        assert_eq!(
            res.outs.into_iter().next().unwrap(),
            PathBuf::from("./my/file.ex")
        );

        _m1.assert();
        _m2.assert();
    }

    #[tokio::test]
    async fn fails_when_rules_depend_on_unknown_rules() {
        // 1. Set up the directories and side-effects

        let rule_store_root = assert_fs::TempDir::new().unwrap();
        // let rule_store_root = rule_store_root.into_persistent();
        dbg!(&rule_store_root.path());

        let invocation_dir = assert_fs::TempDir::new().unwrap();
        // let invocation_dir = invocation_dir.into_persistent();
        dbg!(&invocation_dir.path());

        let test_file = invocation_dir.child("file.ex");
        test_file.touch().unwrap();

        let mock_url = mockito::server_url().parse::<Url>().unwrap();
        let mock_port = mock_url.port().unwrap().to_string();
        // NOTE(@ostera): This mock will be used to download a test_rule as if it was from the
        // internet.
        let _m1 = mockito::mock("GET", "/rule_with_dep.js")
            .with_status(200)
            .with_body(
                include_str!("./fixtures/rule_with_dep.js")
                    .replace("{URL}", &mockito::server_url())
                    .as_bytes(),
            )
            .create();

        let _m2 = mockito::mock("GET", "/dep_rule.js")
            .with_status(400)
            .with_body(
                include_str!("./fixtures/dep_rule.js")
                    .replace("{URL}", &mockito::server_url())
                    .as_bytes(),
            )
            .create();

        // 2. Configure Warp and create all the dependencies to the RuleExecutor

        let config = Config::builder()
            .rule_store_root(rule_store_root.path().into())
            .public_rule_store_url(mock_url)
            .invocation_dir(invocation_dir.path().into())
            .build()
            .unwrap();

        let rule_store = RuleStore::new(&config).into();
        let target_registry: Arc<TargetRegistry> = TargetRegistry::default().into();
        let task_results = TaskResults::new(target_registry.clone()).into();
        let ctx = SharedJsContext::new(target_registry.clone(), task_results, rule_store);

        // 3. Create our planning inputs

        let goal = Goal::Build;
        let target = Arc::new("./my/file.ex".into());
        let target_id = target_registry.register_target(&target);
        let c_target = ConcreteTarget::new(goal, target_id, target.clone(), "".into());

        let mut config = rule::Config::default();
        config.insert("name".into(), Value::Target((*target).clone()));
        config.insert(
            "srcs".into(),
            Value::List(vec![Value::File("./my/file.ex".into())]),
        );

        let env = Default::default();
        let sig = Signature::builder()
            .rule("rule_with_dep".into())
            .target(c_target)
            .config(config)
            .build()
            .unwrap();

        let deps = Dependencies::default();

        // 4. Create the executor and run!

        let mut exec = JsRuleExecutor::new(ctx).unwrap();
        let err = exec.execute(&env, &sig, &deps).await.unwrap_err();

        let expected_module_name = format!(
            "file://{}/http/127.0.0.1:{mock_port}/rule_with_dep.js",
            rule_store_root.path().to_string_lossy(),
        );
        assert_matches!(
            err,
            RuleExecutorError::ModuleResolutionError { module_name, .. }
                if module_name == expected_module_name
        );

        _m1.assert();
        _m2.assert();
    }
}
