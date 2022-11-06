mod commands;
mod reporter;

use commands::*;
use opentelemetry::global::shutdown_tracer_provider;
use std::path::PathBuf;
use structopt::StructOpt;
use tokio::fs;
use tracing::*;
use tracing_subscriber::prelude::__tracing_subscriber_SubscriberExt;
use warp_core::WarpEngine;

#[derive(StructOpt, Debug, Clone)]
#[structopt(
    name = "warp",
    setting = structopt::clap::AppSettings::ColoredHelp,
    about = "A simple, fast, and correct build system for modern polyglot teams"
)]
struct Warp {
    #[structopt(subcommand, help = "the command to run")]
    cmd: Option<Command>,

    #[structopt(
        long = "user",
        help = "the user running this command. This will default to $USER"
    )]
    user: Option<String>,

    #[structopt(
        long = "enable-tracing",
        help = "turns on the tracing options for remote debugging"
    )]
    enable_tracing: bool,

    #[structopt(
        long = "warp-home",
        help = "the root directory for the Warp global configuration"
    )]
    warp_home: Option<String>,
}

impl Warp {
    async fn run(mut self) -> Result<(), anyhow::Error> {
        let t0 = std::time::Instant::now();

        human_panic::setup_panic!(Metadata {
            name: "warp".into(),
            version: env!("CARGO_PKG_VERSION").into(),
            authors: "Leandro Ostera <leandro@abstractmachines.dev>".into(),
            homepage: "https://warp.build".into(),
        });

        let enable_tracing = self.enable_tracing;
        if enable_tracing {
            let tracer = opentelemetry_jaeger::new_pipeline()
                .install_batch(opentelemetry::runtime::Tokio)?;

            // Create a tracing layer with the configured tracer
            let telemetry = tracing_opentelemetry::layer().with_tracer(tracer);

            // Use the tracing subscriber `Registry`, or any other subscriber
            // that impls `LookupSpan`
            let subscriber = tracing_subscriber::Registry::default().with(telemetry);

            // Trace executed code
            tracing::subscriber::set_global_default(subscriber)?;
        }

        env_logger::Builder::new()
            .filter_level(log::LevelFilter::Off)
            .format_timestamp_micros()
            .format_module_path(false)
            .parse_env("WARP_LOG")
            .try_init()
            .unwrap();

        let root_span = trace_span!("Warp::run");

        let results = async move {
            let result = self.start(t0).await;

            match result {
                Ok(()) => (),
                Err(ref err) => error!("{:?}", &err),
            };

            result
        }
        .instrument(root_span)
        .await;

        if enable_tracing {
            shutdown_tracer_provider();
        }

        results
    }

    #[tracing::instrument(name = "Warp::start")]
    async fn start(&mut self, t0: std::time::Instant) -> Result<(), anyhow::Error> {
        let cwd = fs::canonicalize(PathBuf::from(&".")).await.unwrap();

        let warp = WarpEngine::builder()
            .start_time(t0)
            .invocation_dir(cwd)
            .warp_root(self.warp_home.clone())
            .current_user(self.user.clone().unwrap_or_else(whoami::username))
            .build()?;

        match &self.cmd {
            Some(Command::New(x)) => return x.run(warp).await,
            Some(Command::Init(x)) => return x.run(warp).await,
            Some(Command::Setup(x)) => return x.run(&warp).await,
            _ => (),
        };

        let mut warp = warp.initialize().await?;

        let result = self
            .cmd
            .take()
            .unwrap_or_else(|| Command::Build(BuildCommand::all()))
            .run(&mut warp)
            .await;

        warp.shutdown().await?;

        result
    }
}

#[derive(StructOpt, Debug, Clone)]
enum Command {
    Build(BuildCommand),
    Hash(HashCommand),
    Info(InfoCommand),
    Init(InitCommand),
    Lift(LiftCommand),
    New(NewCommand),
    Run(RunCommand),
    Setup(SetupCommand),
    Shell(ShellCommand),
    Test(TestCommand),
}

impl Command {
    #[tracing::instrument(name = "Command::run", skip(self, warp))]
    async fn run(self, warp: &mut WarpEngine) -> Result<(), anyhow::Error> {
        match self {
            Command::Build(x) => x.run(warp).await,
            Command::Hash(x) => x.run(warp).await,
            Command::Info(x) => x.run(warp).await,
            Command::Lift(x) => x.run(warp).await,
            Command::Run(x) => x.run(warp).await,
            Command::Shell(x) => x.run(warp).await,
            Command::Test(x) => x.run(warp).await,
            Command::New(_) | Command::Init(_) | Command::Setup(_) => {
                panic!("New, Init, and Setup should be handled especially since they consume the Engine")
            }
        }
    }
}

#[tokio::main(flavor = "current_thread")]
async fn main() -> Result<(), anyhow::Error> {
    Warp::from_args().run().await.map(|_| ())?;
    Ok(())
}
