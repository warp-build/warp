use crate::reporter::*;
use anyhow::*;
use std::path::Path;
use std::sync::Arc;
use structopt::StructOpt;
use warp_core::*;

#[derive(StructOpt, Debug, Clone)]
#[structopt(
    name = "test",
    setting = structopt::clap::AppSettings::ColoredHelp,
    about = "Incrementally run one or many tests"
)]
pub struct TestGoal {
    #[structopt(
        help = r"The test to run.

A path to a directory with a warp file, followed by a colon
and the name of the label to be tested.

Example: //my/library:my_test

Use //... to build the entire project.
",
        default_value = "//..."
    )]
    label: String,

    #[structopt(
        help = r"The amount of workers to use to execute any necessary build tasks.",
        short = "w",
        long = "max-workers"
    )]
    max_workers: Option<usize>,
}

impl TestGoal {
    pub async fn run(
        self,
        _build_started: std::time::Instant,
        _cwd: &Path,
        workspace: Workspace,
        event_channel: Arc<EventChannel>,
    ) -> Result<(), anyhow::Error> {
        let label: Label = (&workspace.aliases)
            .get(&self.label)
            .cloned()
            .unwrap_or_else(|| Label::from_path(&workspace.paths.workspace_root, &self.label));

        let worker_limit = self.max_workers.unwrap_or_else(num_cpus::get);

        let warp = BuildExecutor::from_workspace(workspace, worker_limit);

        let status_reporter = StatusReporter::new(event_channel.clone());
        let (_result, ()) = futures::future::join(
            warp.build(
                label.clone(),
                event_channel.clone(),
                TargetFilter::OnlyTests,
            ),
            status_reporter.run(label.clone()),
        )
        .await;

        Err(anyhow!("There was no target to run."))
    }
}
