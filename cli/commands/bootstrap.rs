use super::*;
use structopt::StructOpt;
use warp_core::{Goal, WarpDriveMarkII};

#[derive(StructOpt, Debug, Clone)]
#[structopt(
    name = "bootstrap",
    setting = structopt::clap::AppSettings::ColoredHelp,
    about = "Bootstrap a target in this Workspace",
)]
pub struct BootstrapCommand {
    #[structopt(flatten)]
    flags: Flags,
}

impl BootstrapCommand {
    pub async fn run(self) -> Result<(), anyhow::Error> {
        let mut warp = WarpDriveMarkII::new(self.flags.into()).await?;

        let targets = vec![
            "./tricorders/beam/mix.exs",
        ];

        let _results = warp.execute(Goal::Bootstrap, targets).await?;

        Ok(())
    }
}
