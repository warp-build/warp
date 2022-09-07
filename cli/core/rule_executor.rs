use super::*;
use dashmap::DashMap;
use deno_core::anyhow::{bail, Error};
use deno_core::futures::FutureExt;
use deno_core::resolve_import;
use deno_core::ModuleLoader;
use deno_core::ModuleSource;
use deno_core::ModuleSourceFuture;
use deno_core::ModuleSpecifier;
use deno_core::ModuleType;
use deno_core::*;
use fxhash::FxHashSet;
use std::collections::HashMap;
use std::path::PathBuf;
use std::pin::Pin;
use std::rc::Rc;
use std::sync::Arc;
use thiserror::*;
use tokio::fs;
use tracing::*;

pub struct ExecutionResult {
    pub actions: Vec<Action>,
    pub outs: FxHashSet<PathBuf>,
    pub srcs: FxHashSet<PathBuf>,
    pub run_script: Option<RunScript>,
}

pub struct NetModuleLoader;

/// NOTE(@ostera): this feature copied from `deno-simple-module-loader`:
///     https://github.com/andreubotella/deno-simple-module-loader)
impl ModuleLoader for NetModuleLoader {
    fn resolve(
        &self,
        specifier: &str,
        referrer: &str,
        _is_main: bool,
    ) -> Result<ModuleSpecifier, Error> {
        Ok(resolve_import(specifier, referrer)?)
    }

    fn load(
        &self,
        module_specifier: &ModuleSpecifier,
        _maybe_referrer: Option<ModuleSpecifier>,
        _is_dyn_import: bool,
    ) -> Pin<Box<ModuleSourceFuture>> {
        let module_specifier = module_specifier.clone();
        let string_specifier = module_specifier.to_string();
        async {
            let bytes = match module_specifier.scheme() {
                "http" | "https" => reqwest::get(module_specifier).await?.bytes().await?,
                "file" => {
                    let path = match module_specifier.to_file_path() {
                        Ok(path) => path,
                        Err(_) => bail!("Invalid file URL."),
                    };
                    fs::read(path).await?.into()
                }
                schema => bail!("Invalid schema {}", schema),
            };

            // Strip BOM
            let code = if bytes.starts_with(&[0xEF, 0xBB, 0xBF]) {
                bytes.slice(3..)
            } else {
                bytes
            }
            .to_vec()
            .into_boxed_slice();

            Ok(ModuleSource {
                code,
                module_type: ModuleType::JavaScript,
                module_url_specified: string_specifier.clone(),
                module_url_found: string_specifier,
            })
        }
        .boxed_local()
    }
}

static JS_SNAPSHOT: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/JS_SNAPSHOT.bin"));

#[derive(Error, Debug)]
pub enum LoadError {
    #[error("The module name `{module_name}` is invalid: {reason:?}")]
    BadModuleName {
        module_name: String,
        reason: deno_core::url::ParseError,
    },

    #[error("The module `{module_name}` had issues importing some other files: {reason:?}")]
    ModuleResolutionError {
        module_name: String,
        reason: anyhow::Error,
    },

    #[error("The module name `{module_name}` could not be evaluated: {reason:?}")]
    ModuleEvaluationError {
        module_name: String,
        reason: anyhow::Error,
    },

    #[error("Something went wrong.")]
    Unknown,
}

#[derive(Error, Debug)]
pub enum RuleExecutorError {
    #[error(transparent)]
    ExecutableTargetError(ExecutableTargetError),

    #[error("Could not find declared outputs for target {label:?}")]
    MissingDeclaredOutputs { label: Label },

    #[error(transparent)]
    DenoExecutionError(AnyError),

    #[error("Execution Error for {}\nTarget: {target:#?}\n\nError: {err:?}", label.to_string())]
    ExecutionError {
        err: anyhow::Error,
        label: Label,
        target: Box<Target>,
        rule: Rule,
    },

    #[error("Something went wrong.")]
    Unknown,

    #[error(transparent)]
    ConfigExpanderError(ConfigExpanderError),
}

/// The Rule Execution Environment abstracts away the communication between the Dependency Graph
/// and the BuildScript rules.
///
/// It is responsible updating the RuleManager and the ToolchainManager, by detecting when new
/// rules and toolchains are defined, as well as computing the actions and outputs that are
/// expected of both rules and toolchains.
///
pub struct RuleExecutor {
    runtime: deno_core::JsRuntime,
    pub rule_map: Arc<DashMap<String, Rule>>,
    pub action_map: Arc<DashMap<Label, Vec<Action>>>,
    pub output_map: Arc<DashMap<Label, Vec<PathBuf>>>,
    pub provides_map: Arc<DashMap<Label, HashMap<String, String>>>,
    pub run_script_map: Arc<DashMap<Label, RunScript>>,
}

impl RuleExecutor {
    pub fn new() -> RuleExecutor {
        let rule_map = Arc::new(DashMap::new());
        let action_map = Arc::new(DashMap::new());
        let output_map = Arc::new(DashMap::new());
        let provides_map = Arc::new(DashMap::new());
        let run_script_map = Arc::new(DashMap::new());

        let extension: deno_core::Extension = {
            let action_map = action_map.clone();
            let output_map = output_map.clone();
            let provides_map = provides_map.clone();
            let rule_map = Arc::clone(&rule_map);
            let run_script_map = run_script_map.clone();

            let inner_state = InnerState {
                action_map,
                output_map,
                provides_map,
                rule_map,
                run_script_map,
            };

            Extension::builder()
                .ops(vec![
                    rule_exec_env_ffi::op_ctx_actions_copy::decl(),
                    rule_exec_env_ffi::op_ctx_actions_declare_outputs::decl(),
                    rule_exec_env_ffi::op_ctx_actions_declare_run_script::decl(),
                    rule_exec_env_ffi::op_ctx_actions_exec::decl(),
                    rule_exec_env_ffi::op_ctx_actions_run_shell::decl(),
                    rule_exec_env_ffi::op_ctx_actions_write_file::decl(),
                    rule_exec_env_ffi::op_ctx_declare_provides::decl(),
                    rule_exec_env_ffi::op_ctx_download::decl(),
                    rule_exec_env_ffi::op_ctx_extract::decl(),
                    rule_exec_env_ffi::op_ctx_fetch_provides::decl(),
                    rule_exec_env_ffi::op_ctx_set_permissions::decl(),
                    rule_exec_env_ffi::op_file_filename::decl(),
                    rule_exec_env_ffi::op_file_parent::decl(),
                    rule_exec_env_ffi::op_file_with_extension::decl(),
                    rule_exec_env_ffi::op_label_name::decl(),
                    rule_exec_env_ffi::op_label_path::decl(),
                    rule_exec_env_ffi::op_rule_new::decl(),
                    rule_exec_env_ffi::op_toolchain_new::decl(),
                ])
                .state(move |state| {
                    state.put(inner_state.clone());
                    Ok(())
                })
                .build()
        };

        let rt_options = deno_core::RuntimeOptions {
            startup_snapshot: Some(deno_core::Snapshot::Static(JS_SNAPSHOT)),
            module_loader: Some(Rc::new(NetModuleLoader)),
            extensions: vec![extension, deno_console::init()],
            ..Default::default()
        };
        let runtime = deno_core::JsRuntime::new(rt_options);

        RuleExecutor {
            action_map,
            output_map,
            provides_map,
            rule_map,
            run_script_map,
            runtime,
        }
    }

    #[tracing::instrument(name = "RuleExecutor::update_provide_map", skip(self, node, store))]
    pub async fn update_provide_map(
        &self,
        node: &ExecutableTarget,
        store: &Store,
    ) -> Result<(), anyhow::Error> {
        let abs_node_path = store.absolute_path_by_node(&node).await?;

        let provides = if let Some(provides) = self.provides_map.get(&node.label) {
            let mut new_provides = HashMap::new();
            for (k, v) in provides.value() {
                new_provides.insert(
                    k.clone(),
                    abs_node_path.join(v).to_str().unwrap().to_string(),
                );
            }
            new_provides
        } else {
            HashMap::new()
        };

        self.provides_map.insert(node.label.clone(), provides);

        Ok(())
    }

    pub fn clear(&mut self) {
        self.action_map.clear();
        self.output_map.clear();
        self.provides_map.clear();
    }

    pub async fn load(
        &mut self,
        module_name: &str,
        module_code: Option<String>,
    ) -> Result<(), LoadError> {
        let mod_specifier =
            url::Url::parse(module_name).map_err(|reason| LoadError::BadModuleName {
                module_name: module_name.to_string(),
                reason,
            })?;
        trace!("loading {:?}", &module_name);
        let mod_id = self
            .runtime
            .load_side_module(&mod_specifier, module_code)
            .await
            .map_err(|reason| LoadError::ModuleResolutionError {
                module_name: module_name.to_string(),
                reason,
            })?;
        trace!("evaluating {:?}", &module_name);
        let eval_future = self.runtime.mod_evaluate(mod_id);
        self.runtime.run_event_loop(false).await.map_err(|reason| {
            LoadError::ModuleEvaluationError {
                module_name: module_name.to_string(),
                reason,
            }
        })?;
        let _ = eval_future.await.unwrap();
        self.runtime
            .get_module_namespace(mod_id)
            .map_err(|reason| LoadError::ModuleEvaluationError {
                module_name: module_name.to_string(),
                reason,
            })?;
        trace!("done with {:?}", &module_name);
        Ok(())
    }

    pub fn setup(&mut self) -> Result<(), RuleExecutorError> {
        trace!("Running prelude.js");
        self.runtime
            .execute_script("<prelude>", include_str!("prelude.js"))
            .map_err(RuleExecutorError::DenoExecutionError)?;

        Ok(())
    }

    pub async fn execute(
        &self,
        env: &ExecutionEnvironment,
        rule: &Rule,
        target: &Target,
        transitive_deps: &[Dependency],
    ) -> Result<ExecutionResult, RuleExecutorError> {
        let config = ConfigExpander
            .expand(&rule, &target)
            .await
            .map_err(RuleExecutorError::ConfigExpanderError)?;

        let compute_program = r#"

(() => {
    Warp.Targets.compute({
      label: "{LABEL_NAME}",
      rule: "{RULE_NAME}",
      cfg: {CONFIG},
      deps: {DEPS},
      transitiveDeps: {TRANSITIVE_DEPS},
      platform: {PLATFORM},
    });
})();

        "#;

        let script_name = format!("<target: {:?}>", &target.label.to_string());
        self.runtime
            .execute_script(&script_name, &compute_program)
            .map_err(|err| RuleExecutorError::ExecutionError {
                err,
                label: target.label.clone(),
                target: Box::new(target.clone()),
                rule: rule.clone(),
            })?;

        let actions = self
            .action_map
            .get(&target.label)
            .map(|entry| entry.value().clone())
            .unwrap_or_default();

        let outs: FxHashSet<PathBuf> = self
            .output_map
            .get(&target.label)
            .ok_or(RuleExecutorError::MissingDeclaredOutputs {
                label: target.label.clone(),
            })?
            .iter()
            .cloned()
            .collect();

        let run_script: Option<RunScript> =
            self.run_script_map.get(&target.label).map(|rs| rs.clone());

        let srcs: FxHashSet<PathBuf> = config
            .get_file_lists()
            .unwrap_or_default()
            .iter()
            .cloned()
            .collect();

        self.clear();

        Ok(ExecutionResult {
            actions,
            outs,
            srcs,
            run_script,
        })
    }
}

#[cfg(tests)]
mod tests {}
