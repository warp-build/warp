
use crate::flags::Flags;
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
    #[structopt(flatten)]
    flags: Flags,

    #[structopt(
        help = r"The test to run.

A path to a directory with a warp file, followed by a colon
and the name of the target to be tested.

Example: ./tests/my_test.ex

Use @all to test the entire project.
",
        default_value = "@all"
    )]
    target: String,

    #[structopt(
        help = r"The name of the tests to run.

When looking for tests within the target, this test matcher will
help us filter the exact tests you're trying to run.

Examples:
    my_test_case
    group:my_test_group
    171

Leave empty to test everything in the target.
",
        name = "test-matcher"
    )]
    matcher: Vec<String>,
}

impl TestCommand {
    pub async fn run(self) -> Result<(), anyhow::Error> {
        let mut warp = WarpDriveMarkII::new(self.flags.into()).await?;
        let _results = warp.run_test(self.matcher, &[self.target]).await?;
        Ok(())
    }
}
