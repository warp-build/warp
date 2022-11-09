use super::*;
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

    #[structopt(flatten)]
    flags: Flags,
}

impl TestCommand {
    pub async fn run(self, warp: &mut WarpEngine) -> Result<(), anyhow::Error> {
        let label: Label = if let Some(label) = warp.workspace.aliases.get(&self.label) {
            label.clone()
        } else {
            let mut label: Label = self.label.parse()?;
            label.set_workspace(&warp.workspace.paths.workspace_root);
            label
        };

        let status_reporter = StatusReporter::new(
            warp.event_channel.clone(),
            self.flags.show_cache_hits,
            Goal::Build,
        );

        let (_results, ()) = futures::future::join(
            warp.execute(
                &[label.clone()],
                self.flags
                    .into_build_opts()
                    .with_goal(Goal::Test)
                    .with_target_filter(TargetFilter::OnlyTests),
            ),
            status_reporter.run(&[label.clone()]),
        )
        .await;

        Ok(())
    }
}
