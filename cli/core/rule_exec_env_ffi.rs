use super::*;
use anyhow::anyhow;
use dashmap::DashMap;
use deno_core::error::AnyError;
use deno_core::*;
use serde::*;
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;
use tracing::*;

#[derive(Default, Clone, Debug)]
pub struct InnerState {
    pub id: uuid::Uuid,
    pub rule_map: Arc<DashMap<String, Rule>>,
    pub run_script_map: Arc<DashMap<Label, RunScript>>,
    pub action_map: Arc<DashMap<Label, Vec<Action>>>,
    pub output_map: Arc<DashMap<Label, Vec<PathBuf>>>,
    pub provides_map: Arc<DashMap<Label, HashMap<String, String>>>,
}

#[op]
pub fn op_label_path(str: String) -> Result<String, AnyError> {
    Ok(Label::new(&str).path().to_str().unwrap().to_string())
}

#[op]
pub fn op_label_name(str: String) -> Result<String, AnyError> {
    Ok(Label::new(&str).name())
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
    let final_path = path.with_extension(args.ext.strip_prefix('.').unwrap());
    Ok(final_path.to_str().unwrap().to_string())
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct DeclareRunScript {
    label: String,
    run_script: String,
    env: HashMap<String, String>,
}

#[op]
pub fn op_ctx_actions_declare_run_script(
    state: &mut OpState,
    args: DeclareRunScript,
) -> Result<(), AnyError> {
    let inner_state = state.try_borrow_mut::<InnerState>().unwrap();
    let run_script_map = &inner_state.run_script_map;

    let label = Label::new(&args.label);
    let run_script = match run_script_map.get(&label) {
        None => PathBuf::from(args.run_script),
        Some(_entry) => panic!("RunScript already declared for: {:?}", &label),
    };
    run_script_map.insert(
        label,
        RunScript {
            run_script,
            env: args.env,
        },
    );
    Ok(())
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
            new_outs
        }
    };
    output_map.insert(label, new_outs);
    Ok(())
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct DeclareProvides {
    label: String,
    provides: std::collections::HashMap<String, String>,
}

#[op]
pub fn op_ctx_declare_provides(state: &mut OpState, args: DeclareProvides) -> Result<(), AnyError> {
    let inner_state = state.try_borrow_mut::<InnerState>().unwrap();
    let provides_map = &inner_state.provides_map;

    let label = Label::new(&args.label);
    let new_provides = match provides_map.get(&label) {
        None => args.provides,
        Some(_) => return Err(anyhow!("You can't specify provides twice!")),
    };
    provides_map.insert(label, new_provides);

    Ok(())
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct FetchProvides {
    label: String,
}

#[op]
pub fn op_ctx_fetch_provides(
    state: &mut OpState,
    args: FetchProvides,
) -> Result<HashMap<String, String>, AnyError> {
    let inner_state = state.try_borrow_mut::<InnerState>().unwrap();
    let provides_map = &inner_state.provides_map;

    let label = Label::new(&args.label);
    let provides = match provides_map.get(&label) {
        None => {
            panic!("Undefined provides for label: {:?}", &label)
        }
        Some(provides) => provides.clone(),
    };

    Ok(provides)
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct SetPermissions {
    label: String,
    file: PathBuf,
    executable: bool,
}

#[op]
pub fn op_ctx_set_permissions(state: &mut OpState, args: SetPermissions) -> Result<(), AnyError> {
    let inner_state = state.try_borrow_mut::<InnerState>().unwrap();
    let action_map = &inner_state.action_map;

    let label = Label::new(&args.label);
    let action = Action::set_permissions(args.file, args.executable);
    let new_actions = if let Some(entry) = action_map.get(&label) {
        let last_actions = entry.value();
        let mut new_actions = vec![];
        new_actions.extend(last_actions.to_vec());
        new_actions.push(action);
        new_actions
    } else {
        vec![action]
    };

    action_map.insert(label, new_actions);

    Ok(())
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct Extract {
    label: String,
    src: PathBuf,
    dst: PathBuf,
}

#[op]
pub fn op_ctx_extract(state: &mut OpState, args: Extract) -> Result<(), AnyError> {
    let inner_state = state.try_borrow_mut::<InnerState>().unwrap();
    let action_map = &inner_state.action_map;

    let label = Label::new(&args.label);
    let action = Action::extract(args.src, args.dst);
    let new_actions = if let Some(entry) = action_map.get(&label) {
        let last_actions = entry.value();
        let mut new_actions = vec![];
        new_actions.extend(last_actions.to_vec());
        new_actions.push(action);
        new_actions
    } else {
        vec![action]
    };

    action_map.insert(label, new_actions);

    Ok(())
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct Download {
    label: String,
    url: String,
    sha1: String,
    output: PathBuf,
}

#[op]
pub fn op_ctx_download(state: &mut OpState, args: Download) -> Result<(), AnyError> {
    let inner_state = state.try_borrow_mut::<InnerState>().unwrap();
    let action_map = &inner_state.action_map;

    let label = Label::new(&args.label);
    let action = Action::download(args.url, args.sha1, args.output);
    let new_actions = if let Some(entry) = action_map.get(&label) {
        let last_actions = entry.value();
        let mut new_actions = vec![];
        new_actions.extend(last_actions.to_vec());
        new_actions.push(action);
        new_actions
    } else {
        vec![action]
    };

    action_map.insert(label, new_actions);

    Ok(())
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct RunShell {
    label: String,
    script: String,
    env: std::collections::HashMap<String, String>,
    needs_tty: bool,
}

#[op]
pub fn op_ctx_actions_run_shell(state: &mut OpState, args: RunShell) -> Result<(), AnyError> {
    let inner_state = state.try_borrow_mut::<InnerState>().unwrap();
    let action_map = &inner_state.action_map;

    let label = Label::new(&args.label);
    let action = Action::run_shell(args.script, args.env, args.needs_tty);
    let new_actions = if let Some(entry) = action_map.get(&label) {
        let last_actions = entry.value();
        let mut new_actions = vec![];
        new_actions.extend(last_actions.to_vec());
        new_actions.push(action);
        new_actions
    } else {
        vec![action]
    };

    action_map.insert(label, new_actions);

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
    env: std::collections::HashMap<String, String>,
    cmd: String,
    args: Vec<String>,
    cwd: Option<String>,
    needs_tty: bool,
}

#[op]
pub fn op_ctx_actions_exec(state: &mut OpState, args: Exec) -> Result<(), AnyError> {
    let inner_state = state.try_borrow_mut::<InnerState>().unwrap();
    let action_map = &inner_state.action_map;

    let label = Label::new(&args.label);
    let cwd: Option<PathBuf> = args.cwd.map(PathBuf::from);
    let action = Action::exec(
        PathBuf::from(args.cmd),
        args.args,
        cwd,
        args.env,
        args.needs_tty,
    );
    let new_actions = if let Some(entry) = action_map.get(&label) {
        let last_actions = entry.value();
        let mut new_actions = vec![];
        new_actions.extend(last_actions.to_vec());
        new_actions.push(action);
        new_actions
    } else {
        vec![action]
    };

    action_map.insert(label, new_actions);
    Ok(())
}

#[op]
pub fn op_rule_new(state: &mut OpState, rule: Rule) -> Result<(), AnyError> {
    let inner_state = state.try_borrow_mut::<InnerState>().unwrap();

    debug!("Registering rule: {}", &rule.name);
    inner_state.rule_map.insert(rule.name.to_string(), rule);
    Ok(())
}
