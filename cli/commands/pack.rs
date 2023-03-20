use super::*;
use structopt::StructOpt;
use warp_core::{Goal, Target, WarpDriveMarkII};

#[derive(StructOpt, Debug, Clone)]
#[structopt(
    name = "pack",
    setting = structopt::clap::AppSettings::ColoredHelp,
    about = "Packs a target into a tarball",
)]
pub struct PackCommand {
    #[structopt(help = r"The target to pack.

A path to a directory with a warp file, followed by a colon
and the name of the target to be packed.

Example: ./my/library
")]
    target: String,

    #[structopt(flatten)]
    flags: Flags,
}

impl PackCommand {
    pub async fn run(self) -> Result<(), anyhow::Error> {
        let mut warp = WarpDriveMarkII::new(self.flags.into()).await?;

        let target: Target = self.target.into();

        let _results = warp.execute(Goal::Build, &[target.clone()]).await?;

        let packed_results = warp.pack(target).await?;

        //dbg!(packed_results);

        Ok(())
    }
}
