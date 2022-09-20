use crate::reporter::*;
use anyhow::*;
use std::sync::Arc;
use structopt::StructOpt;
use warp_core::*;

#[derive(StructOpt, Debug, Clone)]
#[structopt(
    name = "info",
    setting = structopt::clap::AppSettings::ColoredHelp,
    about = "Information about a target"
)]
pub struct InfoGoal {
    #[structopt(help = r"The target to get information about.

A path to a directory with a warp file, followed by a colon
and the name of the label to be built.

Example: //my/library:shell
")]
    label: String,

    #[structopt(
        help = r"The amount of workers to use to execute any necessary build tasks.",
        short = "w",
        long = "max-workers"
    )]
    max_workers: Option<usize>,
}

impl InfoGoal {
    pub async fn run(
        self,
        workspace: Workspace,
        event_channel: Arc<EventChannel>,
    ) -> Result<(), anyhow::Error> {
        let label: Label = (&workspace.aliases)
            .get(&self.label)
            .cloned()
            .unwrap_or_else(|| Label::from_path(&workspace.paths.workspace_root, &self.label));

        if label.is_all() {
            return Err(anyhow!(
                "You can't run everything. Please specify a target like this: warp build //my/app"
            ));
        }

        let worker_limit = self.max_workers.unwrap_or_else(num_cpus::get);

        let warp = BuildExecutor::from_workspace(workspace, worker_limit);

        let status_reporter = StatusReporter::new(event_channel.clone());
        let (result, ()) = futures::future::join(
            warp.build(
                label.clone(),
                event_channel.clone(),
                TargetFilter::Everything,
            ),
            status_reporter.run(label.clone()),
        )
        .await;

        if let Some((manifest, target)) = result? {
            dbg!(manifest);
            dbg!(target);
            return Ok(());
        }

        Err(anyhow!("There was no target to get info on."))
    }
}
