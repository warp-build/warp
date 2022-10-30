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
pub struct TestCommand {
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

impl TestCommand {
    pub async fn run(self, warp: &WarpEngine) -> Result<(), anyhow::Error> {
        let label: Label = (&warp.workspace.aliases)
            .get(&self.label)
            .cloned()
            .unwrap_or_else(|| {
                Label::builder()
                    .with_workspace(&warp.workspace)
                    .from_string(&self.label)
                    .unwrap()
            });

        let status_reporter = StatusReporter::new(warp.event_channel.clone());
        let (_results, ()) = futures::future::join(
            warp.execute(
                &[label.clone()],
                BuildOpts {
                    goal: Goal::Test,
                    target_filter: TargetFilter::OnlyTests,
                    concurrency_limit: self.max_workers.unwrap_or_else(num_cpus::get),
                    ..Default::default()
                },
            ),
            status_reporter.run(&[label.clone()]),
        )
        .await;

        Ok(())
    }
}
