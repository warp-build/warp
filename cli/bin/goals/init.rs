use anyhow::*;
use dialoguer::{Input, MultiSelect};
use std::collections::{BTreeMap, HashMap};
use std::path::PathBuf;
use std::process::{Command, Stdio};
use std::sync::Arc;
use structopt::StructOpt;
use tracing::*;
use warp_core::*;

use crate::reporter::StatusReporter;

#[derive(StructOpt, Debug, Clone)]
#[structopt(
    name = "init",
    setting = structopt::clap::AppSettings::ColoredHelp,
    about = "initializes a new workspace"
)]
pub struct InitGoal {
    #[structopt(
        help = r"The amount of workers to use to execute any necessary build tasks.",
        short = "w",
        long = "max-workers"
    )]
    max_workers: Option<usize>,
}

struct Selector {}

impl Selector {
    fn map_select_result_to_items(result: Vec<usize>, items: Vec<&str>) -> Vec<String> {
        result
            .iter()
            .map(|i| items[i.clone()].to_string())
            .collect()
    }

    pub fn show_for_items(
        theme: &dialoguer::theme::ColorfulTheme,
        items: Vec<&str>,
    ) -> Result<Vec<String>, anyhow::Error> {
        let result = MultiSelect::with_theme(theme)
            .items(&items)
            .with_prompt("Please choose the toolchains for your workspace")
            .interact()?;

        let selected_toolchains = Selector::map_select_result_to_items(result, items);

        Ok(selected_toolchains)
    }
}

impl InitGoal {
    pub async fn run(
        &self,
        build_started: std::time::Instant,
        cwd: &PathBuf,
        current_user: String,
        event_channel: Arc<EventChannel>,
    ) -> Result<(), anyhow::Error> {
        let theme = dialoguer::theme::ColorfulTheme::default();

        println!(
            "\nWelcome {}, let's create a Workspace for your crew.\n",
            current_user,
        );

        let workspace_root = std::fs::canonicalize(PathBuf::from("."))?;

        let paths = WorkspacePaths::new(&workspace_root, None, current_user.clone())?;

        let current_dir = workspace_root
            .file_name()
            .unwrap()
            .to_str()
            .unwrap()
            .to_string();

        let workspace_name: String = Input::with_theme(&theme)
            .with_prompt("Workspace name")
            .default(current_dir)
            .show_default(true)
            .interact_text()?;

        let use_git_hooks: bool = Input::with_theme(&theme)
            .with_prompt("Should we run Warp before every commit?")
            .default("yes".to_string())
            .show_default(true)
            .interact()?
            == "yes";

        let mut toolchains_registry = ToolchainsRegistry::new(&paths);

        toolchains_registry.ready().await?;

        let chosen_toolchains =
            Selector::show_for_items(&theme, toolchains_registry.show_toolchains())?;

        println!("\nPreparing...\n");

        let workspace_config = WorkspaceConfig::builder()
            .name(workspace_name)
            .use_git_hooks(use_git_hooks)
            .build()?;

        let workspace_file = WorkspaceFile::builder()
            .aliases({
                let mut map = BTreeMap::new();
                map.insert(
                    "erlang".to_string(),
                    "https://pkgs.warp.build/toolchains/erlang".to_string(),
                );
                map
            })
            .workspace(workspace_config)
            .toolchains(toolchains_registry.config_from_chosen_toolchains(&chosen_toolchains))
            .remote_workspaces({
                let mut map = BTreeMap::new();

                map.insert(
                    "tools.warp.build".to_string(),
                    RemoteWorkspaceFile {
                        github: Some("warp-build/tools.warp.build".to_string()),
                        git_ref: Some("main".to_string()),
                        ..RemoteWorkspaceFile::default()
                    },
                );

                map
            })
            .build()?;

        workspace_file.write(&workspace_root).await?;

        let workspace = Workspace::builder()
            .current_user(current_user)
            .paths(paths)
            .from_file(workspace_file)
            .await
            .unwrap()
            .build()?;

        let lifters =
            toolchains_registry.get_lifters_from_chosen_toolchains(&chosen_toolchains, &workspace);

        if !lifters.is_empty() {
            let worker_limit = self.max_workers.unwrap_or_else(num_cpus::get);
            let local_outputs_root = workspace.paths.local_outputs_root.clone();
            let warp = BuildExecutor::from_workspace(workspace, worker_limit);

            let status_reporter = StatusReporter::new(event_channel.clone());
            let (lifters, ()) = futures::future::join(
                warp.build(&lifters, event_channel.clone(), BuildOpts::default()),
                status_reporter.run(&lifters),
            )
            .await;
            for (manifest, target) in lifters? {
                let mut provides_env = manifest.env_map();

                if let Some(RunScript { run_script, env }) = target.run_script {
                    let path = local_outputs_root.join(&run_script);
                    debug!("Running default run_script ({:?})", &path);
                    provides_env.extend(env);

                    self.run_cmd(
                        build_started,
                        cwd,
                        manifest.label.clone(),
                        path.clone(),
                        provides_env.clone(),
                        &[],
                    )?;
                }
            }
        }

        println!(
            r#"We are ready, captain!

Here's a few commands for you to try:

* Use `warp login` to set up the shared caching and remote builds
* Use `warp build` to build your workspace incrementally
* Use `warp test` to test your workspace incrementally
* Use `warp --help` to learn more about other useful commands

Build fast and prosper 🖖
"#
        );

        Ok(())
    }

    fn run_cmd(
        &self,
        build_started: std::time::Instant,
        cwd: &PathBuf,
        label: Label,
        bin: PathBuf,
        mut env: HashMap<String, String>,
        args: &[String],
    ) -> Result<(), Error> {
        let mut cmd = Command::new(bin);

        let extra_paths = env.get("PATH").cloned().unwrap_or_else(|| "".to_string());
        env.remove("PATH");
        env.insert("PATH".to_string(), format!("/bin:/usr/bin:{}", extra_paths));

        cmd.envs(&env);

        cmd.current_dir(cwd)
            .stdin(Stdio::inherit())
            .stderr(Stdio::inherit())
            .stdout(Stdio::inherit())
            .args(args);

        debug!("Spawning {:?}", &cmd);
        let mut proc = cmd.spawn().unwrap();

        let t1 = std::time::Instant::now();
        let delta = t1.saturating_duration_since(build_started).as_millis();
        debug!("Spawned program in {:?}ms", delta);

        trace!("Waiting on {:?}", &cmd);
        proc.wait()
            .map(|_| ())
            .context(format!("Error executing {}", &label.to_string()))?;

        trace!("Exited with status: {}", cmd.status()?);

        Ok(())
    }
}
