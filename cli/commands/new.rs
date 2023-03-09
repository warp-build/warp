use anyhow::*;
use dialoguer::{Input, MultiSelect};
use std::collections::BTreeMap;
use structopt::StructOpt;
use warp_core::*;

#[derive(StructOpt, Debug, Clone)]
#[structopt(
    name = "new",
    setting = structopt::clap::AppSettings::ColoredHelp,
    about = "creates a new workspace"
)]
pub struct NewCommand {
    #[structopt(help = r"The name of the Workspace to be created.")]
    workspace_name: String,
}

impl NewCommand {
    pub async fn run(&self, warp: WarpEngine) -> Result<(), anyhow::Error> {
        let theme = dialoguer::theme::ColorfulTheme::default();

        println!(
            "\nWelcome {}, let's create a Workspace for your crew.\n",
            warp.current_user,
        );

        let current_dir = warp.invocation_dir.join(&self.workspace_name);
        let paths = WorkspacePaths::new(&current_dir, None, warp.current_user.clone())?;

        let workspace_name: String = Input::with_theme(&theme)
            .with_prompt("Workspace name")
            .default(self.workspace_name.to_string())
            .show_default(true)
            .interact_text()?;

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

        workspace_file.write(&current_dir).await?;

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
