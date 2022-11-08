use crate::reporter::*;
use anyhow::*;
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

    #[structopt(
        help = r"Whether to show all the cache hit entries in the build output.",
        long = "show-cache-hits"
    )]
    show_cache_hits: bool,
    #[structopt(
        help = r"EXPERIMENTAL: this flag will ignore the cache and always rebuild",
        long = "experimental-stream-analyzer-outputs"
    )]
    experimental_stream_analyzer_outputs: bool,
}

impl TestCommand {
    pub async fn run(self, warp: &mut WarpEngine) -> Result<(), anyhow::Error> {
        let label: Label = if let Some(label) = (&warp.workspace.aliases).get(&self.label) {
            label.clone()
        } else {
            let mut label: Label = self.label.parse()?;
            label.set_workspace(&warp.workspace.paths.workspace_root);
            label
        };

        let status_reporter = StatusReporter::new(
            warp.event_channel.clone(),
            self.show_cache_hits,
            Goal::Build,
        );

        let (_results, ()) = futures::future::join(
            warp.execute(
                &[label.clone()],
                BuildOpts {
                    goal: Goal::Test,
                    target_filter: TargetFilter::OnlyTests,
                    concurrency_limit: self.max_workers.unwrap_or_else(num_cpus::get),
                    experimental_stream_analyzer_outputs: self.experimental_stream_analyzer_outputs,
                    ..Default::default()
                },
            ),
            status_reporter.run(&[label.clone()]),
        )
        .await;

        Ok(())
    }
}
