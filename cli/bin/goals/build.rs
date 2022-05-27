use std::io;
use std::io::Write;
use std::sync::Arc;
use structopt::StructOpt;
use tracing::*;
use zap_core::*;

#[derive(StructOpt, Debug, Clone)]
#[structopt(
    name = "build",
    setting = structopt::clap::AppSettings::ColoredHelp,
    about = "Build a target in this Workspace",
)]
pub struct BuildGoal {
    #[structopt(
        help = r"The target to build.

A path to a directory with a zap file, followed by a colon
and the name of the label to be built.

Example: //my/library:lib

Use //... to build the entire project.
",
        default_value = "//..."
    )]
    target: String,

    #[structopt(
        help = r"The amount of workers to use to execute any necessary build tasks.",
        short = "w",
        long = "max-workers"
    )]
    max_workers: Option<usize>,
}

impl BuildGoal {
    pub fn all() -> BuildGoal {
        BuildGoal {
            target: "//...".to_string(),
            max_workers: None,
        }
    }

    #[tracing::instrument(name = "BuildGoal::run", skip(workspace))]
    pub async fn run(
        self,
        workspace: Workspace,
        event_channel: Arc<EventChannel>,
    ) -> Result<(), anyhow::Error> {
        let target: Label = self.target.into();
        debug!("Host: {}", guess_host_triple::guess_host_triple().unwrap());
        debug!("Target: {}", &target.to_string());

        let name = if target.is_all() {
            "workspace".to_string()
        } else {
            target.to_string()
        };

        let worker_limit = self.max_workers.unwrap_or(num_cpus::get());

        let zap = BuildExecutor::from_workspace(workspace, worker_limit);

        let (result, ()) =
            futures::future::join(zap.build(target, event_channel.clone()), async move {
                let event_channel = event_channel.clone();
                println!("🔨 Building {}...", name);
                loop {
                    tokio::time::sleep(std::time::Duration::from_millis(1)).await;
                    if let Some(event) = event_channel.recv() {
                        println!("{:?}", event);
                        if event == zap_core::Event::BuildCompleted {
                            return;
                        }
                    }
                }
            })
            .await;

        result
    }
}
