use anyhow::*;
use dialoguer::{Input, MultiSelect};
use std::collections::{BTreeMap, HashMap};
use std::path::PathBuf;
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

        let toolchains_registry = ToolchainsRegistry::fetch(&paths, event_channel.clone()).await?;

        let chosen_toolchains = MultiSelect::with_theme(&theme)
            .items(
                &toolchains_registry
                    .toolchains()
                    .iter()
                    .map(|t| format!("{} {}", t.id.language, t.id.version))
                    .collect::<Vec<String>>(),
            )
            .with_prompt("Please choose the toolchains for your workspace")
            .interact()?;

        let toolchains: Vec<Toolchain> = toolchains_registry
            .toolchains()
            .iter()
            .enumerate()
            .filter(|(idx, _)| chosen_toolchains.contains(idx))
            .map(|(_idx, t)| t.clone())
            .collect();

        let toolchains = toolchains_registry.flatten_dependencies(toolchains);

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
            .toolchains({
                let mut map = BTreeMap::new();
                for toolchain in &toolchains {
                    map.insert(
                        toolchain.id.language.to_string(),
                        TryFrom::try_from(toolchain.config.clone()).unwrap(),
                    );
                }
                map
            })
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

        let lifters: Vec<Label> = toolchains.iter().cloned().flat_map(|t| t.lifter).collect();
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
                CommandRunner::builder()
                    .cwd(cwd.to_path_buf())
                    .manifest(manifest)
                    .target(target)
                    .stream_outputs(true)
                    .sandboxed(false)
                    .args(vec![])
                    .build()?
                    .run()
                    .await?;
            }
        }

        println!(
            r#"We are ready, here's a few commands for you to try:

  warp build   build your workspace incrementally
  warp test    test your workspace incrementally

Build fast and prosper 🖖
"#
        );

        Ok(())
    }
}
