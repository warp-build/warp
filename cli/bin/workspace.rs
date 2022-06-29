use anyhow::*;
use std::path::PathBuf;
use structopt::StructOpt;
use warp_core::*;

#[derive(StructOpt, Debug, Clone)]
#[structopt(
    name = "workspace",
    setting = structopt::clap::AppSettings::ColoredHelp,
    about = "workspace related goals"
)]
pub enum WorkspaceGoal {
    #[structopt(
        name = "info",
        setting = structopt::clap::AppSettings::ColoredHelp,
        about = "print information about this workspace"
    )]
    Info,
}

impl WorkspaceGoal {
    pub async fn run(self, config: WarpConfig) -> Result<(), anyhow::Error> {
        match self {
            WorkspaceGoal::Info => {
                let mut warp = WarpWorker::new(config)?;
                warp.load(&PathBuf::from(&".")).await?;
                let workspace = warp.workspace();
                println!("Name: {:?}", workspace.name());
                println!("Workspace Root: {:?}", workspace.root());
                println!();
                println!("Global Warp Directories:");
                println!("* Cache: {:?}", warp.config().cache_root);
                println!("* Rules: {:?}", warp.config().rules_root);
                println!("* Toolchains: {:?}", warp.config().toolchains_root);
                println!();
                println!("Local Warp Directories:");
                println!("* Outputs: {:?}", workspace.outputs_root());
                println!("* Rules: {:?}", workspace.rules_root());
                println!("* Sandbox: {:?}", workspace.sandbox_root());
                println!("* Toolchains: {:?}", workspace.toolchains_root());
                println!();
                println!("# of Targets: {:?}", workspace.targets().len());
                Ok(())
            }
        }
    }
}
