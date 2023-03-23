mod commands;
pub mod flags;
mod reporter;

use commands::*;
use structopt::StructOpt;
use tracing::{error, log};

#[derive(StructOpt, Debug, Clone)]
#[structopt(
    name = "warp",
    setting = structopt::clap::AppSettings::ColoredHelp,
    about = "A simple, fast, and correct build system for modern polyglot teams"
)]
struct Warp {
    #[structopt(subcommand, help = "the command to run")]
    cmd: Option<Command>,
}

impl Warp {
    async fn run(mut self) -> Result<(), anyhow::Error> {
        human_panic::setup_panic!(Metadata {
            name: "warp".into(),
            version: env!("CARGO_PKG_VERSION").into(),
            authors: "Leandro Ostera <leandro@warp.build>".into(),
            homepage: "https://warp.build".into(),
        });

        env_logger::Builder::new()
            .filter_level(log::LevelFilter::Off)
            .format_timestamp_micros()
            .format_module_path(false)
            .parse_env("WARP_LOG")
            .try_init()
            .unwrap();

        let result = self
            .cmd
            .take()
            .unwrap_or_else(|| Command::Build(BuildCommand::all()))
            .run()
            .await;

        if let Err(ref err) = result {
            error!("{:?}", &err);
        };

        result
    }
}

#[derive(StructOpt, Debug, Clone)]
enum Command {
    Build(BuildCommand),
    Run(RunCommand),
    Setup(SetupCommand),
    Test(TestCommand),
    Bootstrap(BootstrapCommand),
    Pack(PackCommand),
}

impl Command {
    async fn run(self) -> Result<(), anyhow::Error> {
        match self {
            Command::Bootstrap(x) => x.run().await,
            Command::Build(x) => x.run().await,
            Command::Pack(x) => x.run().await,
            Command::Run(x) => x.run().await,
            Command::Setup(s) => s.run().await,
            Command::Test(x) => x.run().await,
        }
    }
}

#[tokio::main(flavor = "current_thread")]
async fn main() -> Result<(), anyhow::Error> {
    Warp::from_args().run().await.map(|_| ())
}
