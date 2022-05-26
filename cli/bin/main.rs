mod goals;
use goals::*;

use opentelemetry::global::shutdown_tracer_provider;
use std::path::PathBuf;
use std::sync::Arc;
use structopt::StructOpt;
use tracing::*;
use tracing_subscriber::prelude::__tracing_subscriber_SubscriberExt;
use zap_core::*;

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
        long = "enable-tracing",
        help = "turns on the tracing options for remote debugging"
    )]
    enable_tracing: bool,

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
            .parse_env("ZAP_LOG")
            .try_init()
            .unwrap();

        let root_span = trace_span!("Zap::run");

        async move {
            let result = self.start().await;

            match result {
                Ok(()) => (),
                Err(ref err) => error!("{:?}", &err),
            };

            let t1 = t0.elapsed().as_millis();
            if !self.quiet {
                println!("\x1B[1000D\x1B[K\r⚡ done in {}ms", t1);
            }

            result
        }
        .instrument(root_span)
        .await
    }

    #[tracing::instrument(name = "Zap::start")]
    async fn start(&self) -> Result<(), anyhow::Error> {
        let event_channel = Arc::new(EventChannel::new());

        let cwd = PathBuf::from(&".");
        let workspace: Workspace =
            WorkspaceBuilder::build(cwd, self.zap_home.clone(), self.user.clone())?;

        if let Some(cmd) = self.cmd.clone() {
            cmd
        } else {
            Goal::Build(BuildGoal::all())
        }
        .run(workspace, event_channel)
        .await
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
    #[tracing::instrument(name = "Goal::run", skip(self, workspace))]
    async fn run(
        self,
        workspace: Workspace,
        event_channel: Arc<EventChannel>,
    ) -> Result<(), anyhow::Error> {
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
            Goal::Build(x) => x.run(workspace, event_channel).await,
            Goal::Clean(x) => x.run(workspace, event_channel).await,
            Goal::Info(x) => x.run(workspace, event_channel).await,
            Goal::Run(x) => x.run(workspace, event_channel).await,
        }
    }
}

#[tokio::main(flavor = "current_thread")]
async fn main() -> Result<(), anyhow::Error> {
    Zap::from_args().run().await.map(|_| ())?;
    shutdown_tracer_provider();
    Ok(())
}
