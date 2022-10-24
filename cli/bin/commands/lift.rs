use structopt::StructOpt;

#[derive(StructOpt, Debug, Clone)]
#[structopt(
    name = "lift",
    setting = structopt::clap::AppSettings::ColoredHelp,
    about = "wraps a rebar3, mix, or erlang.mk project in a Warp build target"
)]
pub struct LiftCommand {}

impl LiftCommand {
    pub fn run(self) -> Result<(), anyhow::Error> {
        Ok(())
    }
}
