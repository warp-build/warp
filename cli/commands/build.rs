use super::*;
use structopt::StructOpt;
use warp_core::{Goal, WarpDriveMarkII};

#[derive(StructOpt, Debug, Clone)]
#[structopt(
    name = "build",
    setting = structopt::clap::AppSettings::ColoredHelp,
    about = "Build a target in this Workspace",
)]
pub struct BuildCommand {
    #[structopt(
        help = r"The target to build.

A path to a directory with a warp file, followed by a colon
and the name of the target to be built.

Example: ./my/library

Use @all to build the entire project.
",
        default_value = "@all"
    )]
    target: String,

    #[structopt(flatten)]
    flags: Flags,
}

impl BuildCommand {
    pub fn all() -> BuildCommand {
        BuildCommand {
            target: "@all".to_string(),
            flags: Flags::default(),
        }
    }

    pub async fn run(self) -> Result<(), anyhow::Error> {
        let mut warp = WarpDriveMarkII::new(self.flags.into()).await?;
        let target = self.target.into();
        let _results = warp.execute(Goal::Build, &[target]).await?;
        Ok(())
    }
}
