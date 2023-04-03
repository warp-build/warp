use crate::executor::actions::Action;
use crate::model::{Rule, RunScript, TaskId, TestRunner};
use crate::rules::FfiContext;
use deno_core::error::AnyError;
use deno_core::*;
use fxhash::*;
use serde::*;
use std::collections::HashMap;
use std::path::PathBuf;
use tracing::*;

#[op]
pub fn op_env_var(var: String) -> Result<String, AnyError> {
    Ok(std::env::var(var).unwrap_or_default())
}

#[op]
pub fn op_target_dir(state: &mut OpState, task: TaskId) -> Result<String, AnyError> {
    let ctx = state.borrow::<FfiContext>();
    let task = ctx.task_registry.get(task);
    let target = ctx.target_registry.get_concrete_target(task.target_id());
    Ok(target.dir().to_str().unwrap().to_string())
}

#[op]
pub fn op_target_path(state: &mut OpState, task: TaskId) -> Result<String, AnyError> {
    let ctx = state.borrow::<FfiContext>();
    let task = ctx.task_registry.get(task);
    let target = ctx.target_registry.get_concrete_target(task.target_id());
    let path = target.path().to_str().unwrap().to_string();

    Ok(if path.is_empty() {
        ".".to_string()
    } else {
        let cleaned = path.replace("./", "");
        if cleaned.ends_with('/') {
            cleaned[0..cleaned.len() - 1].to_string()
        } else {
            cleaned
        }
    })
}

#[op]
pub fn op_target_name(state: &mut OpState, task: TaskId) -> Result<String, AnyError> {
    let ctx = state.borrow::<FfiContext>();
    let task = ctx.task_registry.get(task);
    let target = ctx.target_registry.get_concrete_target(task.target_id());
    Ok(target.name().to_string())
}

#[op]
pub fn op_file_parent(filepath: String) -> Result<String, AnyError> {
    let path = PathBuf::from(filepath);
    let parent = path
        .parent()
        .and_then(|p| p.to_str())
        .map(|p| p.to_string())
        .unwrap();
    Ok(if parent.is_empty() {
        ".".to_string()
    } else {
        parent
    })
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
pub struct DeclareTestRunner {
    task: TaskId,
    test_runner: String,
    env: HashMap<String, String>,
}

#[op]
pub fn op_ctx_actions_declare_test_runner(
    state: &mut OpState,
    args: DeclareTestRunner,
) -> Result<(), AnyError> {
    let ctx = state.borrow::<FfiContext>();
    let test_runner_map = &ctx.test_runner_map;

    let task = args.task;
    let test_runner = match test_runner_map.get(&task) {
        None => PathBuf::from(args.test_runner),
        Some(_entry) => panic!("TestRunner already declared for: {:?}", &task),
    };
    test_runner_map.insert(
        task,
        TestRunner {
            test_runner,
            env: args.env,
        },
    );
    Ok(())
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct DeclareRunScript {
    task: TaskId,
    run_script: String,
    env: HashMap<String, String>,
}

#[op]
pub fn op_ctx_actions_declare_run_script(
    state: &mut OpState,
    args: DeclareRunScript,
) -> Result<(), AnyError> {
    let ctx = state.borrow::<FfiContext>();
    let run_script_map = &ctx.run_script_map;

    let task = args.task;
    let run_script = match run_script_map.get(&task) {
        None => PathBuf::from(args.run_script),
        Some(_entry) => panic!("RunScript already declared for: {:?}", &task),
    };
    run_script_map.insert(
        task,
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
    task: TaskId,
    outs: Vec<String>,
}

#[op]
pub fn op_ctx_actions_declare_outputs(
    state: &mut OpState,
    args: DeclareOutputs,
) -> Result<(), AnyError> {
    let ctx = state.borrow::<FfiContext>();
    let output_map = &ctx.output_map;

    let task = args.task;
    let outs: Vec<PathBuf> = args.outs.iter().map(PathBuf::from).collect();
    let new_outs = match output_map.get(&task) {
        None => outs,
        Some(entry) => {
            let last_outs = entry.value();
            let mut new_outs = vec![];
            new_outs.extend(last_outs.to_vec());
            new_outs.extend(outs);
            new_outs
        }
    };
    output_map.insert(task, new_outs);
    Ok(())
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct DeclareEnv {
    task: TaskId,
    env: FxHashMap<String, String>,
}

#[op]
pub fn op_ctx_declare_env(state: &mut OpState, args: DeclareEnv) -> Result<(), AnyError> {
    let ctx = state.borrow::<FfiContext>();
    let env_map = &ctx.shell_env_map;

    let task = args.task;
    env_map.insert(task, args.env);

    Ok(())
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct DeclareProvides {
    task: TaskId,
    provides: FxHashMap<String, String>,
}

#[op]
pub fn op_ctx_declare_provides(state: &mut OpState, args: DeclareProvides) -> Result<(), AnyError> {
    let ctx = state.borrow::<FfiContext>();
    let provides_map = &ctx.provides_map;

    provides_map.insert(args.task, args.provides);

    Ok(())
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct FetchProvides {
    task: TaskId,
}

#[op]
pub fn op_ctx_fetch_provides(
    state: &mut OpState,
    args: FetchProvides,
) -> Result<FxHashMap<String, String>, AnyError> {
    let ctx = state.borrow::<FfiContext>();
    let provides_map = &ctx.provides_map;

    let provides = match provides_map.get(&args.task) {
        None => {
            panic!("Undefined provides for task: {:?}", &args.task)
        }
        Some(provides) => provides.clone(),
    };

    Ok(provides)
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct SetPermissions {
    task: TaskId,
    file: PathBuf,
    executable: bool,
}

#[op]
pub fn op_ctx_set_permissions(state: &mut OpState, args: SetPermissions) -> Result<(), AnyError> {
    let ctx = state.borrow::<FfiContext>();
    let action_map = &ctx.action_map;

    let task = args.task;
    let action = Action::set_permissions(args.file, args.executable);
    let new_actions = if let Some(entry) = action_map.get(&task) {
        let last_actions = entry.value();
        let mut new_actions = vec![];
        new_actions.extend(last_actions.to_vec());
        new_actions.push(action);
        new_actions
    } else {
        vec![action]
    };

    action_map.insert(task, new_actions);

    Ok(())
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct Extract {
    task: TaskId,
    src: PathBuf,
    dst: PathBuf,
}

#[op]
pub fn op_ctx_extract(state: &mut OpState, args: Extract) -> Result<(), AnyError> {
    let ctx = state.borrow::<FfiContext>();
    let action_map = &ctx.action_map;

    let task = args.task;
    let action = Action::extract(args.src, args.dst);
    let new_actions = if let Some(entry) = action_map.get(&task) {
        let last_actions = entry.value();
        let mut new_actions = vec![];
        new_actions.extend(last_actions.to_vec());
        new_actions.push(action);
        new_actions
    } else {
        vec![action]
    };

    action_map.insert(task, new_actions);

    Ok(())
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct VerifyChecksum {
    task: TaskId,
    file: PathBuf,
    sha1: String,
}

#[op]
pub fn op_ctx_verify_checksum(state: &mut OpState, args: VerifyChecksum) -> Result<(), AnyError> {
    let ctx = state.borrow::<FfiContext>();
    let action_map = &ctx.action_map;

    let task = args.task;
    let action = Action::verify_checksum(args.file, args.sha1);
    let new_actions = if let Some(entry) = action_map.get(&task) {
        let last_actions = entry.value();
        let mut new_actions = vec![];
        new_actions.extend(last_actions.to_vec());
        new_actions.push(action);
        new_actions
    } else {
        vec![action]
    };

    action_map.insert(task, new_actions);

    Ok(())
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct Download {
    task: TaskId,
    url: String,
    sha1: String,
    output: PathBuf,
}

#[op]
pub fn op_ctx_download(state: &mut OpState, args: Download) -> Result<(), AnyError> {
    let ctx = state.borrow::<FfiContext>();
    let action_map = &ctx.action_map;

    let task = args.task;
    let action = Action::download(args.url, args.sha1, args.output);
    let new_actions = if let Some(entry) = action_map.get(&task) {
        let last_actions = entry.value();
        let mut new_actions = vec![];
        new_actions.extend(last_actions.to_vec());
        new_actions.push(action);
        new_actions
    } else {
        vec![action]
    };

    action_map.insert(task, new_actions);

    Ok(())
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct RunShell {
    task: TaskId,
    script: String,
    env: std::collections::HashMap<String, String>,
    needs_tty: bool,
}

#[op]
pub fn op_ctx_actions_run_shell(state: &mut OpState, args: RunShell) -> Result<(), AnyError> {
    let ctx = state.borrow::<FfiContext>();
    let action_map = &ctx.action_map;

    let task = args.task;
    let action = Action::run_shell(args.script, args.env, args.needs_tty);
    let new_actions = if let Some(entry) = action_map.get(&task) {
        let last_actions = entry.value();
        let mut new_actions = vec![];
        new_actions.extend(last_actions.to_vec());
        new_actions.push(action);
        new_actions
    } else {
        vec![action]
    };

    action_map.insert(task, new_actions);

    Ok(())
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct WriteFile {
    task: TaskId,
    data: String,
    dst: String,
}

#[op]
pub fn op_ctx_actions_write_file(state: &mut OpState, args: WriteFile) -> Result<(), AnyError> {
    let ctx = state.borrow::<FfiContext>();
    let action_map = &ctx.action_map;

    let task = args.task;
    let action = Action::write_file(args.data, PathBuf::from(args.dst));
    let new_actions = if let Some(entry) = action_map.get(&task) {
        let last_actions = entry.value();
        let mut new_actions = vec![];
        new_actions.extend(last_actions.to_vec());
        new_actions.push(action);
        new_actions
    } else {
        vec![action]
    };

    action_map.insert(task, new_actions);

    Ok(())
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct CopyFile {
    task: TaskId,
    src: String,
    dst: String,
}

#[op]
pub fn op_ctx_actions_copy(state: &mut OpState, args: CopyFile) -> Result<(), AnyError> {
    let ctx = state.borrow::<FfiContext>();
    let action_map = &ctx.action_map;

    let task = args.task;
    let action = Action::copy(PathBuf::from(args.src), PathBuf::from(args.dst));
    let new_actions = if let Some(entry) = action_map.get(&task) {
        let last_actions = entry.value();
        let mut new_actions = vec![];
        new_actions.extend(last_actions.to_vec());
        new_actions.push(action);
        new_actions
    } else {
        vec![action]
    };

    action_map.insert(task, new_actions);
    Ok(())
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct Exec {
    task: TaskId,
    env: std::collections::HashMap<String, String>,
    cmd: String,
    args: Vec<String>,
    cwd: Option<String>,
    needs_tty: bool,
}

#[op]
pub fn op_ctx_actions_exec(state: &mut OpState, args: Exec) -> Result<(), AnyError> {
    let ctx = state.borrow::<FfiContext>();
    let action_map = &ctx.action_map;

    let task = args.task;
    let cwd: Option<PathBuf> = args.cwd.map(PathBuf::from);
    let action = Action::exec(
        PathBuf::from(args.cmd),
        args.args,
        cwd,
        args.env,
        args.needs_tty,
    );
    let new_actions = if let Some(entry) = action_map.get(&task) {
        let last_actions = entry.value();
        let mut new_actions = vec![];
        new_actions.extend(last_actions.to_vec());
        new_actions.push(action);
        new_actions
    } else {
        vec![action]
    };

    action_map.insert(task, new_actions);
    Ok(())
}

#[op]
pub fn op_rule_new(state: &mut OpState, rule: Rule) -> Result<(), AnyError> {
    let ctx = state.borrow::<FfiContext>();

    debug!("Registering rule: {}", &rule.name());
    ctx.rule_map.insert(rule.name().to_string(), rule);
    Ok(())
}
