mod goals;
use goals::*;

use log::*;
use structopt::StructOpt;
use zap_core::*;
use std::path::PathBuf;

#[derive(StructOpt, Debug, Clone)]
#[structopt(
    name = "zap",
    setting = structopt::clap::AppSettings::ColoredHelp,
    about = "A simple, fast, and correct build system for modern polyglot teams"
)]
struct Zap {
    #[structopt(subcommand, help = "the command to run")]
    cmd: Option<Goal>,

    #[structopt(short = "q", long = "quiet", help = "turn off all logs")]
    quiet: bool,

    #[structopt(
        long = "user",
        help = "the user running this command. This will default to $USER"
    )]
    user: Option<String>,

    #[structopt(
        long = "zap-home",
        help = "the root directory for the Zap global configuration"
    )]
    zap_home: Option<String>,
}

impl Zap {
    async fn run(self) -> Result<(), anyhow::Error> {
        let t0 = std::time::Instant::now();

        human_panic::setup_panic!(Metadata {
            name: "zap".into(),
            version: env!("CARGO_PKG_VERSION").into(),
            authors: "Leandro Ostera <leandro@abstractmachines.dev>".into(),
            homepage: "https://zap.build".into(),
        });

        env_logger::Builder::new()
            .filter_level(log::LevelFilter::Info)
            .format_timestamp_micros()
            .format_module_path(false)
            .parse_env("ZAP_LOG")
            .try_init()
            .unwrap();

        let cwd = PathBuf::from(&".");
        let workspace: Workspace =
            WorkspaceBuilder::build(cwd, self.zap_home.clone(), self.user.clone())?;

        let cmd = self.cmd.unwrap_or_else(|| Goal::Build(BuildGoal::all()));
        match cmd.run(workspace).await {
            Ok(()) => (),
            Err(err) => error!("{:?}", &err),
        };

        let t1 = t0.elapsed().as_millis();
        if !self.quiet {
            println!("\x1B[1000D\x1B[K\r⚡ done in {}ms", t1);
        }
        Ok(())
    }
}

#[derive(StructOpt, Debug, Clone)]
enum Goal {
    // Cache(CacheGoal),
    // DepGraph(DepGraphGoal),
    // Deps(DepsGoal),
    // Fmt(FmtGoal),
    // Lift(LiftGoal),
    // New(NewGoal),
    // Query(QueryGoal),
    // Rules(RulesGoal),
    // Test(TestGoal),
    // Toolchains(ToolchainGoal),
    // Workspace(WorkspaceGoal),
    Build(BuildGoal),
    Clean(CleanGoal),
    Info(InfoGoal),
    Run(RunGoal),
}

impl Goal {
    async fn run(self, workspace: Workspace) -> Result<(), anyhow::Error> {
        match self {
            // Goal::Cache(x) => x.run(config).await,
            // Goal::DepGraph(x) => x.run(config).await,
            // Goal::Deps(x) => x.run(),
            // Goal::Fmt(x) => x.run(),
            // Goal::Lift(x) => x.run(),
            // Goal::New(x) => x.run(),
            // Goal::Query(x) => x.run(),
            // Goal::Rules(x) => x.run(config).await,
            // Goal::Targets(x) => x.run(config).await,
            // Goal::Test(x) => x.run(),
            // Goal::Toolchains(x) => x.run(config).await,
            // Goal::Workspace(x) => x.run(config).await,
            Goal::Build(x) => x.run(workspace).await,
            Goal::Clean(x) => x.run(workspace).await,
            Goal::Info(x) => x.run(workspace).await,
            Goal::Run(x) => x.run(workspace).await,
        }
    }
}

#[tokio::main]
async fn main() -> Result<(), anyhow::Error> {
    Zap::from_args().run().await?;
    Ok(())
}