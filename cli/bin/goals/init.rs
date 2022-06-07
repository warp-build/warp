use structopt::StructOpt;

#[derive(StructOpt, Debug, Clone)]
#[structopt(
    name = "init",
    setting = structopt::clap::AppSettings::ColoredHelp,
    about = "initializes a new workspace"
)]
pub struct NewGoal {}

impl NewGoal {
    pub fn run(self) -> Result<(), anyhow::Error> {
        Ok(())
    }
}
