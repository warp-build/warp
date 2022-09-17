// NOTE(@ostera): we need this because the core library has pretty deeply nested
// async types, and without it we'll get an internal compiler error
#![recursion_limit = "256"]

mod goals;
mod reporter;

use goals::*;
use opentelemetry::global::shutdown_tracer_provider;
use std::path::PathBuf;
use std::sync::Arc;
use structopt::StructOpt;
use tokio::fs;
use tracing::*;
use tracing_subscriber::prelude::__tracing_subscriber_SubscriberExt;
use warp_core::Event;
use warp_core::*;

#[derive(StructOpt, Debug, Clone)]
#[structopt(
    name = "warp",
    setting = structopt::clap::AppSettings::ColoredHelp,
    about = "A simple, fast, and correct build system for modern polyglot teams"
)]
struct Warp {
    #[structopt(subcommand, help = "the command to run")]
    cmd: Option<Goal>,

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

        if self.enable_tracing {
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

        async move {
            let result = self.start(t0).await;

            match result {
                Ok(()) => (),
                Err(ref err) => error!("{:?}", &err),
            };

            result
        }
        .instrument(root_span)
        .await
    }

    #[tracing::instrument(name = "Warp::start")]
    async fn start(&mut self, t0: std::time::Instant) -> Result<(), anyhow::Error> {
        let current_user = self.user.clone().unwrap_or_else(whoami::username);
        let warp_home = self.warp_home.clone();

        let event_channel = Arc::new(EventChannel::new());
        event_channel.send(Event::BuildStarted(t0));

        match &self.cmd {
            Some(Goal::Init(x)) => return x.run(current_user.clone(), event_channel).await,
            Some(Goal::Setup(x)) => return x.run(current_user.clone(), event_channel).await,
            _ => (),
        };

        // NOTE(@ostera): save the current directory, to return to it when we're done building
        let cwd = fs::canonicalize(PathBuf::from(&".")).await.unwrap();
        let (root, workspace_file) = WorkspaceFile::find_upwards(&cwd).await?;

        std::env::set_current_dir(&root)?;

        let paths = WorkspacePaths::new(&root, warp_home, current_user.clone())?;

        let workspace = Workspace::builder()
            .current_user(current_user)
            .paths(paths)
            .from_file(workspace_file)
            .await
            .unwrap()
            .build()?;

        let result = self
            .cmd
            .take()
            .unwrap_or_else(|| Goal::Build(BuildGoal::all()))
            .run(workspace, event_channel)
            .await;

        std::env::set_current_dir(&cwd)?;

        result
    }
}

#[derive(StructOpt, Debug, Clone)]
enum Goal {
    Build(BuildGoal),
    Init(InitGoal),
    Run(RunGoal),
    Setup(SetupGoal),
}

impl Goal {
    #[tracing::instrument(name = "Goal::run", skip(self, workspace))]
    async fn run(
        self,
        workspace: Workspace,
        event_channel: Arc<EventChannel>,
    ) -> Result<(), anyhow::Error> {
        match self {
            Goal::Build(x) => x.run(workspace, event_channel).await,
            Goal::Init(x) => x.run(workspace.current_user, event_channel).await,
            Goal::Run(x) => x.run(workspace, event_channel).await,
            Goal::Setup(x) => x.run(workspace.current_user, event_channel).await,
        }
    }
}

#[tokio::main]
async fn main() -> Result<(), anyhow::Error> {
    Warp::from_args().run().await.map(|_| ())?;
    shutdown_tracer_provider();
    Ok(())
}
