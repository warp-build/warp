use structopt::StructOpt;

#[derive(StructOpt, Debug, Clone)]
#[structopt(
    name = "deps",
    setting = structopt::clap::AppSettings::ColoredHelp,
    about = "dependency management"
)]
pub struct DepsCommand {}

impl DepsCommand {
    pub fn run(self) -> Result<(), anyhow::Error> {
        Ok(())
    }
}
