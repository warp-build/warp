use crate::reporter::*;
use std::sync::Arc;
use structopt::StructOpt;
use tracing::*;
use warp_core::*;

#[derive(StructOpt, Debug, Clone)]
#[structopt(
    name = "build",
    setting = structopt::clap::AppSettings::ColoredHelp,
    about = "Build a target in this Workspace",
)]
pub struct BuildGoal {
    #[structopt(
        help = r"The target to build.

A path to a directory with a warp file, followed by a colon
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
        let target: Label = (&workspace.aliases)
            .get(&self.target)
            .cloned()
            .unwrap_or_else(|| self.target.into());
        debug!("Host: {}", guess_host_triple::guess_host_triple().unwrap());
        debug!("Target: {}", &target.to_string());

        let worker_limit = self.max_workers.unwrap_or_else(num_cpus::get);

        let warp = BuildExecutor::from_workspace(workspace, worker_limit);

        let status_reporter = StatusReporter::new(event_channel.clone());
        let (result, ()) = futures::future::join(
            warp.build(target.clone(), event_channel.clone()),
            status_reporter.run(target),
        )
        .await;

        result.map(|_| ())
    }
}
