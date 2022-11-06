use anyhow::*;
use dialoguer::{Input, MultiSelect};
use std::collections::BTreeMap;
use structopt::StructOpt;
use warp_core::*;

use crate::reporter::StatusReporter;

#[derive(StructOpt, Debug, Clone)]
#[structopt(
    name = "init",
    setting = structopt::clap::AppSettings::ColoredHelp,
    about = "initializes a new workspace"
)]
pub struct InitCommand {
    #[structopt(
        help = r"The amount of workers to use to execute any necessary build tasks.",
        short = "w",
        long = "max-workers"
    )]
    max_workers: Option<usize>,
}

impl InitCommand {
    pub async fn run(&self, warp: WarpEngine) -> Result<(), anyhow::Error> {
        let theme = dialoguer::theme::ColorfulTheme::default();

        println!(
            "\nWelcome {}, let's create a Workspace for your crew.\n",
            warp.current_user,
        );

        let paths = WorkspacePaths::new(&warp.invocation_dir, None, warp.current_user.clone())?;

        let current_dir = warp
            .invocation_dir
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

        /*
        let use_git_hooks: bool = Input::with_theme(&theme)
            .with_prompt("Should we run Warp before every commit?")
            .default("yes".to_string())
            .show_default(true)
            .interact()?
            == "yes";
        */

        let toolchains_registry =
            ToolchainsRegistry::fetch(&paths, warp.event_channel.clone()).await?;

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

        let workspace_config = WorkspaceConfigFile::builder()
            .name(workspace_name)
            .build()?;

        let workspace_file = WorkspaceFile::builder()
            .aliases({
                let mut map = BTreeMap::new();
                map.insert(
                    "erlang".to_string(),
                    "https://rules.warp.build/toolchains/erlang".to_string(),
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

        workspace_file.write(&warp.invocation_dir).await?;

        let lifters: Vec<Label> = toolchains.iter().cloned().flat_map(|t| t.lifter).collect();
        if !lifters.is_empty() {
            let mut warp = warp.initialize().await?;
            let status_reporter =
                StatusReporter::new(warp.event_channel.clone(), false, Goal::Build);
            let (lifters, ()) = futures::future::join(
                warp.execute(
                    &lifters,
                    BuildOpts {
                        concurrency_limit: self.max_workers.unwrap_or_else(num_cpus::get),
                        ..BuildOpts::default()
                    },
                ),
                status_reporter.run(&lifters),
            )
            .await;
            lifters?;
            for build_result in warp
                .get_results()
                .iter()
                .filter(|br| br.executable_target.rule.kind.is_runnable())
            {
                let cmd_result = CommandRunner::builder()
                    .cwd(warp.invocation_dir.to_path_buf())
                    .manifest(build_result.target_manifest.clone())
                    .target(build_result.executable_target.clone())
                    .stream_outputs(true)
                    .sandboxed(false)
                    .args(vec![])
                    .build()?
                    .run()
                    .await?;

                let lifted_sigs: LiftedSignatures =
                    serde_json::from_slice(cmd_result.stdout.as_bytes()).map_err(|err| {
                        SignatureStoreError::ParseError {
                            err,
                            json: cmd_result.stdout.clone(),
                        }
                    })?;

                for sig in lifted_sigs.signatures {
                    let mut local_label: LocalLabel = sig.file.clone().into();
                    local_label.set_workspace(&warp.invocation_dir);

                    let source = sig.source.clone();

                    warp.signature_store()
                        .save(&local_label, &source, sig)
                        .await?;
                }
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
