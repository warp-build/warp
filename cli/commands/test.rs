use super::*;
use anyhow::*;
use structopt::StructOpt;
use warp_core::*;

#[derive(StructOpt, Debug, Clone)]
#[structopt(
    name = "test",
    setting = structopt::clap::AppSettings::ColoredHelp,
    about = "Incrementally test the project"
)]
pub struct TestCommand {
    #[structopt(
        help = r"The test to run.

A path to a directory with a warp file, followed by a colon
and the name of the target to be tested.

Example: ./tests/my_test:test_case_name

Use @all to test the entire project.
",
        default_value = "//..."
    )]
    target: String,

    #[structopt(flatten)]
    flags: Flags,
}

impl TestCommand {
    pub async fn run(self) -> Result<(), anyhow::Error> {
        let mut warp = WarpDriveMarkII::new(self.flags.into()).await?;
        let target = self.target.into();
        let _results = warp.execute(Goal::Build, &[target]).await?;
        Ok(())
    }
}
