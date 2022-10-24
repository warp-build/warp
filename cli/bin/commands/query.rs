use structopt::StructOpt;

#[derive(StructOpt, Debug, Clone)]
#[structopt(
    name = "query",
    setting = structopt::clap::AppSettings::ColoredHelp,
    about = "Query the Build Graph"
)]
pub struct QueryCommand {
    #[structopt(help = r"")]
    query: String,
}

impl QueryCommand {
    pub fn run(self) -> Result<(), anyhow::Error> {
        Ok(())
    }
}
