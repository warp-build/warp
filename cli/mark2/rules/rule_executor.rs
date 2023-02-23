use super::*;
use dashmap::DashMap;
use deno_core::anyhow::{bail, Error};
use deno_core::futures::FutureExt;
use deno_core::resolve_import;
use deno_core::Extension;
use deno_core::ModuleLoader;
use deno_core::ModuleSource;
use deno_core::ModuleSourceFuture;
use deno_core::ModuleSpecifier;
use deno_core::ModuleType;
use fxhash::FxHashMap;
use fxhash::FxHashSet;
use std::path::PathBuf;
use std::pin::Pin;
use std::rc::Rc;
use crate::sync::Arc;
use thiserror::*;
use tokio::fs;
use tracing::*;

#[derive(Debug, Default)]
pub struct SharedRuleExecutorState {
    pub rule_map: Arc<DashMap<String, Rule>>,
    pub provides_map: Arc<DashMap<Label, FxHashMap<String, String>>>,
    pub env_map: Arc<DashMap<Label, FxHashMap<String, String>>>,
    pub rule_store: Arc<RuleStore>,
    pub build_results: Arc<BuildResults>,
}

impl SharedRuleExecutorState {
    pub fn new(rule_store: Arc<RuleStore>, build_results: Arc<BuildResults>) -> Self {
        Self {
            rule_store,
            build_results,
            ..Self::default()
        }
    }
}

pub struct ExecutionResult {
    pub actions: Vec<Action>,
    pub target_plan_ended_at: chrono::DateTime<chrono::Utc>,
    pub target_plan_started_at: chrono::DateTime<chrono::Utc>,
    pub env: FxHashMap<String, String>,
    pub outs: FxHashSet<PathBuf>,
    pub provides: FxHashMap<String, PathBuf>,
    pub run_script: Option<RunScript>,
    pub srcs: FxHashSet<SourceInput>,
}

impl Default for ExecutionResult {
    fn default() -> Self {
        Self {
            actions: Default::default(),
            target_plan_ended_at: chrono::Utc::now(),
            target_plan_started_at: chrono::Utc::now(),
            env: Default::default(),
            outs: Default::default(),
            provides: Default::default(),
            run_script: Default::default(),
            srcs: Default::default(),
        }
    }
}

pub struct ComputeTargetProgram;

impl ComputeTargetProgram {
    pub fn as_js_source(
        build_results: Arc<BuildResults>,
        env: &ExecutionEnvironment,
        target: &Target,
        deps: &[LabelId],
        transitive_deps: &[LabelId],
        runtime_deps: &[LabelId],
        rule: &Rule,
        config: &RuleConfig,
    ) -> String {
        let config: serde_json::Value = config.clone().into();

        let deps: serde_json::Value = serde_json::Value::Array(
            deps.iter()
                .flat_map(|dep| build_results.get_manifest(*dep))
                .map(|dep| {
                    let mut map = serde_json::Map::new();
                    map.insert(
                        "name".to_string(),
                        serde_json::Value::String(dep.label.name().into()),
                    );
                    map.insert(
                        "label".to_string(),
                        serde_json::to_value(dep.label.clone()).unwrap(),
                    );
                    map.insert(
                        "srcs".to_string(),
                        serde_json::Value::Array(
                            dep.srcs
                                .iter()
                                .map(|p| serde_json::Value::String(p.to_str().unwrap().to_string()))
                                .collect(),
                        ),
                    );
                    map.insert(
                        "outs".to_string(),
                        serde_json::Value::Array(
                            dep.outs
                                .iter()
                                .map(|p| serde_json::Value::String(p.to_str().unwrap().to_string()))
                                .collect(),
                        ),
                    );
                    serde_json::Value::Object(map)
                })
                .collect(),
        );

        let transitive_deps: serde_json::Value = serde_json::Value::Array(
            transitive_deps
                .iter()
                .flat_map(|dep| build_results.get_manifest(*dep))
                .map(|dep| {
                    let mut map = serde_json::Map::new();
                    map.insert(
                        "name".to_string(),
                        serde_json::Value::String(dep.label.name().into()),
                    );
                    map.insert(
                        "ruleName".to_string(),
                        serde_json::Value::String(dep.rule_name.clone()),
                    );
                    map.insert(
                        "label".to_string(),
                        serde_json::to_value(dep.label.clone()).unwrap(),
                    );
                    map.insert(
                        "srcs".to_string(),
                        serde_json::Value::Array(
                            dep.srcs
                                .iter()
                                .map(|p| serde_json::Value::String(p.to_str().unwrap().to_string()))
                                .collect(),
                        ),
                    );
                    map.insert(
                        "outs".to_string(),
                        serde_json::Value::Array(
                            dep.outs
                                .iter()
                                .map(|p| serde_json::Value::String(p.to_str().unwrap().to_string()))
                                .collect(),
                        ),
                    );
                    serde_json::Value::Object(map)
                })
                .collect(),
        );

        let runtime_deps: serde_json::Value = serde_json::Value::Array(
            runtime_deps
                .iter()
                .flat_map(|dep| build_results.get_manifest(*dep))
                .map(|dep| {
                    let mut map = serde_json::Map::new();
                    map.insert(
                        "name".to_string(),
                        serde_json::Value::String(dep.label.name().into()),
                    );
                    map.insert(
                        "ruleName".to_string(),
                        serde_json::Value::String(dep.rule_name.clone()),
                    );
                    map.insert(
                        "label".to_string(),
                        serde_json::to_value(dep.label.clone()).unwrap(),
                    );
                    map.insert(
                        "srcs".to_string(),
                        serde_json::Value::Array(
                            dep.srcs
                                .iter()
                                .map(|p| serde_json::Value::String(p.to_str().unwrap().to_string()))
                                .collect(),
                        ),
                    );
                    map.insert(
                        "outs".to_string(),
                        serde_json::Value::Array(
                            dep.outs
                                .iter()
                                .map(|p| serde_json::Value::String(p.to_str().unwrap().to_string()))
                                .collect(),
                        ),
                    );
                    serde_json::Value::Object(map)
                })
                .collect(),
        );

        let label: serde_json::Value = serde_json::to_value(target.label.clone()).unwrap();

        let env: serde_json::Value = env.clone().into();

        let program_template = r#"

(() => {
    Warp.Targets.compute({
      label: {LABEL},
      rule: "{RULE_NAME}",
      cfg: {CONFIG},
      deps: {DEPS},
      transitiveDeps: {TRANSITIVE_DEPS},
      runtimeDeps: {RUNTIME_DEPS},
      env: {ENVIRONMENT},
    });
})();

        "#;

        program_template
            .replace("{LABEL}", &label.to_string())
            .replace("{RULE_NAME}", &rule.name)
            .replace("{CONFIG}", &config.to_string())
            .replace("{DEPS}", &deps.to_string())
            .replace("{TRANSITIVE_DEPS}", &transitive_deps.to_string())
            .replace("{RUNTIME_DEPS}", &runtime_deps.to_string())
            .replace("{ENVIRONMENT}", &env.to_string())
    }
}

pub struct NetModuleLoader {
    pub rule_store: Arc<RuleStore>,
}

/// NOTE(@ostera): this feature copied from `deno-simple-module-loader`:
///     https://github.com/andreubotella/deno-simple-module-loader
impl ModuleLoader for NetModuleLoader {
    #[tracing::instrument(name = "NetModuleLoader::resolve", skip(self))]
    fn resolve(
        &self,
        specifier: &str,
        referrer: &str,
        _is_main: bool,
    ) -> Result<ModuleSpecifier, Error> {
        Ok(resolve_import(specifier, referrer)?)
    }

    #[tracing::instrument(
        name = "NetModuleLoader::load",
        skip(self, _maybe_referrer, _is_dyn_import)
    )]
    fn load(
        &self,
        module_specifier: &ModuleSpecifier,
        _maybe_referrer: Option<ModuleSpecifier>,
        _is_dyn_import: bool,
    ) -> Pin<Box<ModuleSourceFuture>> {
        let rule_store = self.rule_store.clone();
        let module_specifier = module_specifier.clone();
        async move {
            let scheme = module_specifier.scheme().to_string();
            let string_specifier = module_specifier.to_string();

            let bytes: Vec<u8> = match scheme.clone().as_str() {
                "http" | "https" => {
                    let (path, _) = rule_store.get(&string_specifier).await?;
                    fs::read(path).await?
                }
                "file" => {
                    let path = match module_specifier.to_file_path() {
                        Ok(path) => path,
                        Err(_) => bail!("Invalid file URL."),
                    };
                    fs::read(path).await?
                }
                schema => bail!("Invalid schema {}", schema),
            };

            // Strip BOM
            let code = if bytes.starts_with(&[0xEF, 0xBB, 0xBF]) {
                bytes.as_slice()[3..].to_vec()
            } else {
                bytes
            }
            .into_boxed_slice();

            let module = ModuleSource {
                code,
                module_type: ModuleType::JavaScript,
                module_url_specified: string_specifier.clone(),
                module_url_found: string_specifier.to_string(),
            };

            Ok(module)
        }
        .boxed_local()
    }
}

static JS_SNAPSHOT: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/JS_SNAPSHOT.bin"));

#[derive(Error, Debug)]
pub enum RuleExecutorError {
    #[error(transparent)]
    ExecutableTargetError(ExecutableTargetError),

    #[error("Could not find declared outputs for target {label:?} in outputs: {outputs:#?}")]
    MissingDeclaredOutputs {
        label: Label,
        outputs: DashMap<Label, Vec<PathBuf>>,
    },

    #[error(transparent)]
    DenoExecutionError(anyhow::Error),

    #[error("Execution Error for {}\nTarget: {target:#?}\n\nError: {err:?}", label.to_string())]
    ExecutionError {
        err: anyhow::Error,
        label: Label,
        target: Box<Target>,
        rule: Rule,
    },

    #[error(transparent)]
    ConfigExpanderError(ConfigExpanderError),

    #[error("Could not load file at {file:?} due to {err:?}")]
    CouldNotReadFile { file: PathBuf, err: std::io::Error },

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

    #[error("Could not find rule {rule_name:?} in the rules map")]
    RuleNotFound { rule_name: String },

    #[error(transparent)]
    ArtifactStoreError(ArtifactStoreError),
}

pub struct RuleExecutor {
    runtime: deno_core::JsRuntime,
    pub rule_map: Arc<DashMap<String, Rule>>,
    pub loaded_rules: FxHashMap<String, Rule>,
    pub loaded_modules: FxHashMap<String, ()>,
    pub action_map: Arc<DashMap<Label, Vec<Action>>>,
    pub output_map: Arc<DashMap<Label, Vec<PathBuf>>>,
    pub provides_map: Arc<DashMap<Label, FxHashMap<String, String>>>,
    pub env_map: Arc<DashMap<Label, FxHashMap<String, String>>>,
    pub run_script_map: Arc<DashMap<Label, RunScript>>,
    pub build_results: Arc<BuildResults>,
    script_count: i32,
}

impl std::fmt::Debug for RuleExecutor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("RuleExecutor")
            .field("rule_map", &self.rule_map)
            .field("loaded_rules", &self.loaded_rules)
            .field("loaded_modules", &self.loaded_modules)
            .field("action_map", &self.action_map)
            .field("output_map", &self.output_map)
            .field("provides_map", &self.provides_map)
            .field("env_map", &self.env_map)
            .field("run_script_map", &self.run_script_map)
            .field("build_results", &self.build_results)
            .field("script_count", &self.script_count)
            .finish()
    }
}

impl RuleExecutor {
    pub fn new(
        shared_state: Arc<SharedRuleExecutorState>,
    ) -> Result<RuleExecutor, RuleExecutorError> {
        let action_map = Arc::new(DashMap::new());
        let output_map = Arc::new(DashMap::new());
        let run_script_map = Arc::new(DashMap::new());

        let extension: deno_core::Extension = {
            let action_map = action_map.clone();
            let output_map = output_map.clone();
            let run_script_map = run_script_map.clone();

            let provides_map = shared_state.provides_map.clone();
            let env_map = shared_state.env_map.clone();
            let rule_map = shared_state.rule_map.clone();

            let inner_state = InnerState {
                id: uuid::Uuid::new_v4(),
                action_map,
                output_map,
                provides_map,
                rule_map,
                run_script_map,
                env_map,
            };

            Extension::builder()
                .ops(vec![
                    rule_exec_env_ffi::op_ctx_actions_copy::decl(),
                    rule_exec_env_ffi::op_ctx_actions_declare_outputs::decl(),
                    rule_exec_env_ffi::op_ctx_actions_declare_run_script::decl(),
                    rule_exec_env_ffi::op_ctx_actions_exec::decl(),
                    rule_exec_env_ffi::op_ctx_actions_run_shell::decl(),
                    rule_exec_env_ffi::op_ctx_actions_write_file::decl(),
                    rule_exec_env_ffi::op_ctx_declare_env::decl(),
                    rule_exec_env_ffi::op_ctx_declare_provides::decl(),
                    rule_exec_env_ffi::op_ctx_download::decl(),
                    rule_exec_env_ffi::op_ctx_extract::decl(),
                    rule_exec_env_ffi::op_ctx_fetch_provides::decl(),
                    rule_exec_env_ffi::op_ctx_set_permissions::decl(),
                    rule_exec_env_ffi::op_ctx_verify_checksum::decl(),
                    rule_exec_env_ffi::op_file_filename::decl(),
                    rule_exec_env_ffi::op_file_parent::decl(),
                    rule_exec_env_ffi::op_file_with_extension::decl(),
                    rule_exec_env_ffi::op_label_name::decl(),
                    rule_exec_env_ffi::op_label_path::decl(),
                    rule_exec_env_ffi::op_rule_new::decl(),
                ])
                .state(move |state| {
                    state.put(inner_state.clone());
                    Ok(())
                })
                .build()
        };

        let rt_options = deno_core::RuntimeOptions {
            startup_snapshot: Some(deno_core::Snapshot::Static(JS_SNAPSHOT)),
            module_loader: Some(Rc::new(NetModuleLoader {
                rule_store: shared_state.rule_store.clone(),
            })),
            // v8_platform: Some(v8::Platform::new_single_threaded(true).make_shared()),
            extensions: vec![extension, deno_console::init()],
            ..Default::default()
        };
        let runtime = deno_core::JsRuntime::new(rt_options);

        let mut rule_executor = RuleExecutor {
            action_map,
            output_map,
            provides_map: shared_state.provides_map.clone(),
            env_map: shared_state.env_map.clone(),
            rule_map: shared_state.rule_map.clone(),
            build_results: shared_state.build_results.clone(),
            run_script_map,
            runtime,
            loaded_rules: FxHashMap::default(),
            loaded_modules: FxHashMap::default(),
            script_count: 0,
        };

        rule_executor.setup()?;

        Ok(rule_executor)
    }

    #[tracing::instrument(
        name = "RuleExecutor::update_provide_map",
        skip(self, node, artifact_store)
    )]
    pub async fn update_provide_map(
        &self,
        node: &ExecutableTarget,
        artifact_store: &ArtifactStore,
    ) -> Result<(), RuleExecutorError> {
        let abs_node_path = artifact_store
            .absolute_path_by_node(node)
            .await
            .map_err(RuleExecutorError::ArtifactStoreError)?;

        let provides = if let Some(provides) = self.provides_map.get(&node.label.to_owned().into())
        {
            let mut new_provides = FxHashMap::default();
            for (k, v) in provides.value() {
                new_provides.insert(
                    k.clone(),
                    abs_node_path.join(v).to_str().unwrap().to_string(),
                );
            }
            new_provides
        } else {
            FxHashMap::default()
        };

        self.provides_map
            .insert(node.label.clone().into(), provides);

        Ok(())
    }

    #[tracing::instrument(name = "RuleExecutor::clear", skip(self))]
    pub fn clear(&mut self) {
        self.action_map.clear();
        self.output_map.clear();
    }

    #[tracing::instrument(name = "RuleExecutor::load_rule", skip(self))]
    pub async fn load_rule(
        &mut self,
        rule_name: &str,
        file: PathBuf,
    ) -> Result<Rule, RuleExecutorError> {
        // NOTE(@ostera): avoid IO and redefining rules if it is already computed in this worker
        if let Some(r) = self.loaded_rules.get(rule_name) {
            return Ok(r.clone());
        }

        self.load_file(file).await?;

        let rule = self
            .rule_map
            .get(rule_name)
            .map(|r| r.value().clone())
            .ok_or_else(|| RuleExecutorError::RuleNotFound {
                rule_name: rule_name.to_string(),
            })?;

        self.loaded_rules
            .insert(rule_name.to_string(), rule.clone());

        Ok(rule)
    }

    #[tracing::instrument(name = "RuleExecutor::load_file", skip(self))]
    pub async fn load_file(&mut self, file: PathBuf) -> Result<(), RuleExecutorError> {
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

    #[tracing::instrument(name = "RuleExecutor::load", skip(self, module_code))]
    pub async fn load(
        &mut self,
        module_name: &str,
        module_code: Option<String>,
    ) -> Result<(), RuleExecutorError> {
        if self.loaded_modules.contains_key(&module_name.to_string()) {
            return Ok(());
        }

        let mod_specifier =
            url::Url::parse(module_name).map_err(|reason| RuleExecutorError::BadModuleName {
                module_name: module_name.to_string(),
                reason,
            })?;

        let mod_id = self
            .runtime
            .load_side_module(&mod_specifier, module_code)
            .await
            .map_err(|reason| RuleExecutorError::ModuleResolutionError {
                module_name: module_name.to_string(),
                reason,
            })?;

        let eval_future = self.runtime.mod_evaluate(mod_id);

        self.runtime.run_event_loop(false).await.map_err(|reason| {
            RuleExecutorError::ModuleEvaluationError {
                module_name: module_name.to_string(),
                reason,
            }
        })?;

        let _ = eval_future.await.unwrap();

        self.runtime
            .get_module_namespace(mod_id)
            .map_err(|reason| RuleExecutorError::ModuleEvaluationError {
                module_name: module_name.to_string(),
                reason,
            })?;

        self.loaded_modules.insert(module_name.to_string(), ());

        Ok(())
    }

    #[tracing::instrument(name = "RuleExecutor::setup", skip(self))]
    pub fn setup(&mut self) -> Result<(), RuleExecutorError> {
        trace!("Running prelude.js");
        self.runtime
            .execute_script("<prelude>", include_str!("prelude.js"))
            .map_err(RuleExecutorError::DenoExecutionError)?;

        Ok(())
    }

    #[tracing::instrument(
        name = "RuleExecutor::execute",
        skip(self, env, rule, target, deps, transitive_deps)
    )]
    pub async fn execute(
        &mut self,
        target_plan_started_at: chrono::DateTime<chrono::Utc>,
        env: &ExecutionEnvironment,
        rule: &Rule,
        target: &Target,
        deps: &[LabelId],
        transitive_deps: &[LabelId],
        runtime_deps: &[LabelId],
    ) -> Result<ExecutionResult, RuleExecutorError> {
        self.script_count += 1;
        trace!("executing script {}", self.script_count);

        let config = ConfigExpander
            .expand(rule, target)
            .await
            .map_err(RuleExecutorError::ConfigExpanderError)?;

        let compute_program = ComputeTargetProgram::as_js_source(
            self.build_results.clone(),
            env,
            target,
            deps,
            transitive_deps,
            runtime_deps,
            rule,
            &config,
        );

        trace!("Executing: {}", compute_program);

        let script_name = format!("<target: {:?}>", &target.label.to_string());

        self.runtime
            .execute_script(&script_name, &compute_program)
            .map_err(|err| RuleExecutorError::ExecutionError {
                err,
                label: target.label.clone(),
                target: Box::new(target.clone()),
                rule: rule.clone(),
            })?;

        trace!("Done!");

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
                outputs: (*self.output_map).clone(),
            })?
            .iter()
            .cloned()
            .collect();

        let run_script: Option<RunScript> =
            self.run_script_map.get(&target.label).map(|rs| rs.clone());

        let srcs: FxHashSet<SourceInput> = config
            .get_file_lists()
            .unwrap_or_default()
            .iter()
            .cloned()
            .map(SourceInput::Path)
            .collect();

        let provides = self
            .provides_map
            .get(&target.label)
            .map(|r| r.value().clone())
            .unwrap_or_default()
            .into_iter()
            .map(|(k, v)| (k, PathBuf::from(v)))
            .collect::<FxHashMap<String, PathBuf>>();

        let env = self
            .env_map
            .get(&target.label)
            .map(|r| r.value().clone())
            .unwrap_or_default()
            .into_iter()
            .collect::<FxHashMap<String, String>>();

        self.clear();

        let target_plan_ended_at = chrono::Utc::now();

        Ok(ExecutionResult {
            target_plan_started_at,
            target_plan_ended_at,
            actions,
            outs,
            srcs,
            run_script,
            provides,
            env,
        })
    }
}
