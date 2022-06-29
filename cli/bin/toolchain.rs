use anyhow::*;
use std::path::PathBuf;
use structopt::StructOpt;
use warp_core::*;

#[derive(StructOpt, Debug, Clone)]
#[structopt(
    name = "toolchains",
    setting = structopt::clap::AppSettings::ColoredHelp,
    about = "manage language toolchains"
)]
pub enum ToolchainGoal {
    #[structopt(
        name = "list-archives",
        setting = structopt::clap::AppSettings::ColoredHelp,
        about = "lists toolchain archives"
    )]
    ListArchives,

    #[structopt(
        name = "list-active",
        setting = structopt::clap::AppSettings::ColoredHelp,
        about = "lists active toolchain"
    )]
    ListActive,

    #[structopt(
        name = "list-available",
        setting = structopt::clap::AppSettings::ColoredHelp,
        about = "lists available toolchain"
    )]
    ListAvailable,
}

impl ToolchainGoal {
    pub async fn run(self, config: WarpConfig) -> Result<(), anyhow::Error> {
        let mut warp = WarpWorker::new(config)?;
        warp.load(&PathBuf::from(&".")).await?;

        match self {
            ToolchainGoal::ListArchives => self.list_archives(&mut warp),
            ToolchainGoal::ListAvailable => self.list_available_toolchains(&mut warp),
            ToolchainGoal::ListActive => self.list_active_toolchains(&mut warp),
        }
    }

    pub fn list_archives(&self, warp: &mut WarpWorker) -> Result<(), anyhow::Error> {
        let toolchain_manager = warp.toolchain_manager();
        let mut archives = toolchain_manager.read().unwrap().archives();

        archives.sort_by_key(|a| a.name().to_string());

        if archives.is_empty() {
            println!("No archives configured.");
        } else {
            println!("Configured Toolchain Archives: ");
            for archive in archives {
                println!(
                    r#"* {name}:
  - url: {url}
  - hash: {hash}
"#,
                    name = archive.name().to_string(),
                    url = archive.url(),
                    hash = archive.hash()
                );
            }
        }
        Ok(())
    }

    pub fn list_available_toolchains(&self, warp: &mut WarpWorker) -> Result<(), anyhow::Error> {
        let toolchain_manager = warp.toolchain_manager();
        let mut toolchains: Vec<String> = toolchain_manager
            .read()
            .unwrap()
            .available_toolchains()
            .iter()
            .map(|t| t.to_string())
            .collect();

        toolchains.sort();

        if toolchains.is_empty() {
            println!("No available toolchains.");
        } else {
            println!("Available Toolchains: ");
            for toolchain in toolchains {
                println!(r#"* {}"#, toolchain);
            }
        }
        Ok(())
    }

    pub fn list_active_toolchains(&self, warp: &mut WarpWorker) -> Result<(), anyhow::Error> {
        let toolchain_manager = warp.toolchain_manager();
        let mut toolchains: Vec<String> = toolchain_manager
            .read()
            .unwrap()
            .active_toolchains()
            .iter()
            .map(|t| t.label().to_string())
            .collect();

        toolchains.sort();

        if toolchains.is_empty() {
            println!("No active toolchains.");
        } else {
            println!("Active Toolchains: ");
            for toolchain in toolchains {
                println!(r#"* {}"#, toolchain);
            }
        }
        Ok(())
    }
}
