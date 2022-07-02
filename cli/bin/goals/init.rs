use dialoguer::Input;
use std::path::PathBuf;
use std::sync::Arc;
use structopt::StructOpt;
use tokio::fs;
use warp_core::*;

#[derive(StructOpt, Debug, Clone)]
#[structopt(
    name = "init",
    setting = structopt::clap::AppSettings::ColoredHelp,
    about = "initializes a new workspace"
)]
pub struct InitGoal {}

impl InitGoal {
    pub async fn run(&self, _event_channel: Arc<EventChannel>) -> Result<(), anyhow::Error> {
        let theme = dialoguer::theme::ColorfulTheme::default();

        println!("\nWelcome skipper, let's create a Workspace for your crew.\n");

        let current_dir = fs::canonicalize(PathBuf::from("."))
            .await?
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

        println!("\nPreparing...\n");

        let workspace_config = WorkspaceConfig::builder()
            .name(workspace_name)
            .use_git_hooks(use_git_hooks)
            .build()?;

        let workspace_file = WorkspaceFile::builder()
            .workspace(workspace_config)
            .build()?;

        let workspace_root = PathBuf::from(".");
        workspace_file.write(&workspace_root).await?;

        println!("We are ready, sir! Build fast and prosper 🖖");

        Ok(())
    }
}
