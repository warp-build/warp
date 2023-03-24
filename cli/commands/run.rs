use crate::flags::Flags;
use anyhow::*;
use structopt::StructOpt;
use warp_core::*;

#[derive(StructOpt, Debug, Clone)]
#[structopt(
    name = "run",
    setting = structopt::clap::AppSettings::ColoredHelp,
    about = "Executes a runnable target"
)]
pub struct RunCommand {
    #[structopt(help = r"The target to run.

A path to a directory with a warp file, followed by a colon
and the name of the target to be ran.

Example: ./app/bin/run.exe

NOTE: not all targets are runnable. Non-runnable targets will
build their dependencies and exit.
")]
    target: String,

    #[structopt(flatten)]
    flags: Flags,

    #[structopt(name = "ARGUMENTS")]
    _args: Vec<String>,
}

impl RunCommand {
    pub async fn run(self) -> Result<(), anyhow::Error> {
        let mut warp = WarpDriveMarkII::new(self.flags.into()).await?;
        let _results = warp.execute(Goal::Build, &[self.target]).await?;
        Ok(())
    }
}
