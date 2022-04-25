use super::*;
use anyhow::Context;
use dashmap::DashMap;
use deno_core::error::AnyError;
use deno_core::*;
use log::*;
use serde::*;
use std::path::PathBuf;
use std::rc::Rc;
use std::sync::Arc;
use std::sync::RwLock;

mod error {
    use thiserror::Error;
    #[derive(Error, Debug)]
    pub enum LoadError {
        #[error("The module name `{module_name}` is invalid: {reason:?}")]
        BadModuleName { module_name: String, reason: String },

        #[error("The module name `{module_name}` could not be resolved: {reason:?}")]
        ModuleResolutionError { module_name: String, reason: String },

        #[error("The module name `{module_name}` could not be evaluated: {reason:?}")]
        ModuleEvaluationError { module_name: String, reason: String },

        #[error("Something went wrong.")]
        Unknown,
    }
}

#[derive(Default, Clone, Debug)]
pub struct InnerState {
    pub paths: WorkspacePaths,
    pub rule_map: Arc<DashMap<String, Rule>>,
    pub toolchain_manager: Arc<RwLock<ToolchainManager>>,
    pub action_map: Arc<DashMap<Label, Vec<Action>>>,
    pub output_map: Arc<DashMap<Label, Vec<PathBuf>>>,
}

#[op]
pub fn op_log(json: serde_json::Value) -> Result<(), AnyError> {
    println!("{}", json.to_string());
    Ok(())
}

#[op]
pub fn op_label_path(str: String) -> Result<String, AnyError> {
    Ok(Label::new(&str).path().to_str().unwrap().to_string())
}

#[op]
pub fn op_file_parent(filepath: String) -> Result<String, AnyError> {
    let path = PathBuf::from(filepath);
    let parent = path.parent().unwrap().to_str().unwrap();
    Ok(parent.to_string())
}

#[op]
pub fn op_file_filename(filepath: String) -> Result<String, AnyError> {
    let path = PathBuf::from(filepath);
    let file_name = path.file_name().unwrap().to_str().unwrap();
    Ok(file_name.to_string())
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct FileWithExtension {
    path: String,
    ext: String,
}

#[op]
pub fn op_file_with_extension(args: FileWithExtension) -> Result<String, AnyError> {
    let path = PathBuf::from(args.path);
    let final_path = path.with_extension(args.ext.strip_prefix(".").unwrap());
    Ok(final_path.to_str().unwrap().to_string())
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct DeclareOutputs {
    label: String,
    outs: Vec<String>,
}

#[op]
pub fn op_ctx_actions_declare_outputs(
    state: &mut OpState,
    args: DeclareOutputs,
) -> Result<(), AnyError> {
    let inner_state = state.try_borrow_mut::<InnerState>().unwrap();
    let output_map = &inner_state.output_map;

    let label = Label::new(&args.label);
    let outs: Vec<PathBuf> = args.outs.iter().map(PathBuf::from).collect();
    let new_outs = match output_map.get(&label) {
        None => outs,
        Some(entry) => {
            let last_outs = entry.value();
            let mut new_outs = vec![];
            new_outs.extend(last_outs.to_vec());
            new_outs.extend(outs);
            debug!("Updating output_map: {:?}", &new_outs);
            new_outs
        }
    };
    output_map.insert(label, new_outs);
    Ok(())
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct WriteFile {
    label: String,
    data: String,
    dst: String,
}

#[op]
pub fn op_ctx_actions_write_file(state: &mut OpState, args: WriteFile) -> Result<(), AnyError> {
    let inner_state = state.try_borrow_mut::<InnerState>().unwrap();
    let action_map = &inner_state.action_map;

    let label = Label::new(&args.label);
    let action = Action::write_file(args.data, PathBuf::from(args.dst));
    let new_actions = if let Some(entry) = action_map.get(&label) {
        let last_actions = entry.value();
        let mut new_actions = vec![];
        new_actions.extend(last_actions.to_vec());
        new_actions.push(action);
        debug!("Updating action_map: {:?}", &new_actions);
        new_actions
    } else {
        vec![action]
    };

    action_map.insert(label, new_actions);

    Ok(())
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct CopyFile {
    label: String,
    src: String,
    dst: String,
}

#[op]
pub fn op_ctx_actions_copy(state: &mut OpState, args: CopyFile) -> Result<(), AnyError> {
    let inner_state = state.try_borrow_mut::<InnerState>().unwrap();
    let action_map = &inner_state.action_map;

    let label = Label::new(&args.label);
    let action = Action::copy(PathBuf::from(args.src), PathBuf::from(args.dst));
    let new_actions = if let Some(entry) = action_map.get(&label) {
        let last_actions = entry.value();
        let mut new_actions = vec![];
        new_actions.extend(last_actions.to_vec());
        new_actions.push(action);
        debug!("Updating action_map: {:?}", &new_actions);
        new_actions
    } else {
        vec![action]
    };

    action_map.insert(label, new_actions);
    Ok(())
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct Exec {
    label: String,
    cmd: String,
    args: Vec<String>,
    cwd: Option<String>,
}

#[op]
pub fn op_ctx_actions_exec(state: &mut OpState, args: Exec) -> Result<(), AnyError> {
    let inner_state = state.try_borrow_mut::<InnerState>().unwrap();
    let action_map = &inner_state.action_map;

    let label = Label::new(&args.label);
    let cwd: Option<PathBuf> = args.cwd.map(PathBuf::from);
    let action = Action::exec(PathBuf::from(args.cmd), args.args, cwd);
    let new_actions = if let Some(entry) = action_map.get(&label) {
        let last_actions = entry.value();
        let mut new_actions = vec![];
        new_actions.extend(last_actions.to_vec());
        new_actions.push(action);
        debug!("Updating action_map: {:?}", &new_actions);
        new_actions
    } else {
        vec![action]
    };

    action_map.insert(label, new_actions);
    Ok(())
}

#[op]
pub fn op_toolchain_new(state: &mut OpState, toolchain: Rule) -> Result<(), AnyError> {
    let inner_state = state.try_borrow_mut::<InnerState>().unwrap();

    trace!("Registering toolchain: {}", &toolchain.name().to_string());
    (*inner_state.toolchain_manager)
        .read()
        .unwrap()
        .register_toolchain(toolchain, inner_state.paths.global_cache_root.clone());

    Ok(())
}

#[op]
pub fn op_rule_new(state: &mut OpState, rule: Rule) -> Result<(), AnyError> {
    let inner_state = state.try_borrow_mut::<InnerState>().unwrap();

    trace!("Registering rule: {}", &rule.name().to_string());
    inner_state.rule_map.insert(rule.name().to_string(), rule);
    Ok(())
}

/// The Rule Execution Environment abstracts away the communication between the Dependency Graph
/// and the BuildScript rules.
///
/// It is responsible updating the RuleManager and the ToolchainManager, by detecting when new
/// rules and toolchains are defined, as well as computing the actions and outputs that are
/// expected of both rules and toolchains.
///
pub struct RuleExecEnv {
    runtime: deno_core::JsRuntime,
    pub rule_map: Arc<DashMap<String, Rule>>,
    pub toolchain_manager: Arc<RwLock<ToolchainManager>>,
    pub action_map: Arc<DashMap<Label, Vec<Action>>>,
    pub output_map: Arc<DashMap<Label, Vec<PathBuf>>>,
}

impl RuleExecEnv {
    pub fn new(workspace: &Workspace) -> RuleExecEnv {
        let toolchain_manager = Arc::new(RwLock::new(ToolchainManager::new(workspace.toolchain_archives.clone())));
        let rule_map = Arc::new(DashMap::new());
        let action_map = Arc::new(DashMap::new());
        let output_map = Arc::new(DashMap::new());

        let extension = {
            let a = toolchain_manager.clone();
            let paths = workspace.paths.clone();
            let inner_rule_map = Arc::clone(&rule_map);
            let action_map = action_map.clone();
            let output_map = output_map.clone();
            let inner_state = InnerState {
                paths: paths,
                toolchain_manager: a,
                rule_map: inner_rule_map,
                action_map: action_map,
                output_map: output_map,
            };
            Extension::builder()
                .ops(vec![
                    op_log::decl(),
                    op_label_path::decl(),
                    op_file_parent::decl(),
                    op_file_filename::decl(),
                    op_file_with_extension::decl(),
                    op_ctx_actions_declare_outputs::decl(),
                    op_ctx_actions_write_file::decl(),
                    op_ctx_actions_copy::decl(),
                    op_ctx_actions_exec::decl(),
                    op_toolchain_new::decl(),
                    op_rule_new::decl(),
                ])
                .state(move |state| {
                    state.put(inner_state.clone());
                    Ok(())
                })
                .build()
        };

        let rt_options = deno_core::RuntimeOptions {
            module_loader: Some(Rc::new(deno_core::FsModuleLoader)),
            extensions: vec![extension],
            ..Default::default()
        };
        let runtime = deno_core::JsRuntime::new(rt_options);

        RuleExecEnv {
            runtime,
            toolchain_manager,
            rule_map,
            action_map,
            output_map,
        }
    }

    pub async fn load(
        &mut self,
        module_name: &str,
        module_code: Option<String>,
    ) -> Result<(), error::LoadError> {
        let mod_specifier =
            url::Url::parse(module_name).map_err(|reason| error::LoadError::BadModuleName {
                module_name: module_name.to_string(),
                reason: reason.to_string(),
            })?;
        trace!("loading {:?}", &module_name);
        let mod_id = self
            .runtime
            .load_side_module(&mod_specifier, module_code)
            .await
            .map_err(|reason| error::LoadError::ModuleResolutionError {
                module_name: module_name.to_string(),
                reason: reason.to_string(),
            })?;
        trace!("evaluating {:?}", &module_name);
        self.runtime.mod_evaluate(mod_id);
        self.runtime.run_event_loop(false).await.map_err(|reason| {
            error::LoadError::ModuleEvaluationError {
                module_name: module_name.to_string(),
                reason: reason.to_string(),
            }
        })?;
        trace!("done with {:?}", &module_name);
        Ok(())
    }

    pub fn setup(&mut self) -> Result<(), anyhow::Error> {
        trace!("Running prelude.js");
        self.runtime
            .execute_script("<prelude>", include_str!("prelude.js"))?;

        Ok(())
    }

    pub fn compute_target(
        &mut self,
        mut computed_target: ComputedTarget,
    ) -> Result<ComputedTarget, anyhow::Error> {
        let label = computed_target.target.label().clone();
        trace!("Sealing Computed Target {:?}", label.to_string());

        let config: serde_json::Value = computed_target.target.config().clone().into();

        let deps = computed_target.deps.clone();
        let transitive_deps: serde_json::Value = serde_json::Value::Array(
            deps.unwrap_or(vec![])
                .iter()
                .map(|dep| {
                    let mut map = serde_json::Map::new();
                    map.insert(
                        "name".to_string(),
                        serde_json::Value::String(dep.label.name().to_string()),
                    );
                    map.insert(
                        "label".to_string(),
                        serde_json::Value::String(dep.label.to_string()),
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
        let compute_program = include_str!("compute_target.js")
            .replace("{LABEL_NAME}", &label.to_string())
            .replace("{RULE_NAME}", computed_target.target.rule().name())
            .replace("{CONFIG}", &config.to_string())
            .replace("{TRANSITIVE_DEPS}", &transitive_deps.to_string());

        trace!("Executing: {}", &compute_program);

        self.runtime.execute_script(
            &format!("<computed_target: {:?}>", &label.to_string()),
            &compute_program,
        )?;

        let actions = self
            .action_map
            .get(&label)
            .map(|entry| entry.value().clone())
            .unwrap_or_default();

        let outs = self
            .output_map
            .get(&label)
            .context(format!(
                "Could not find declared outputs for target  {:?}  - ",
                &label.to_string()
            ))?
            .clone();

        let srcs = if computed_target.target.is_local() {
            computed_target
                .target
                .config()
                .get_file_lists()
                .unwrap_or_default()
        } else {
            vec![]
        };

        computed_target.deps = Some(computed_target.deps.unwrap().to_vec());
        computed_target.srcs = Some(srcs);
        computed_target.outs = Some(outs);
        computed_target.actions = Some(actions);

        computed_target.recompute_hash();

        debug!(
            "Sealed ComputedTarget {} with Hash {:?}",
            label.to_string(),
            computed_target.hash.as_ref().unwrap()
        );

        Ok(computed_target)
    }
}
