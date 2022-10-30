use crate::reporter::*;
use anyhow::*;
use structopt::StructOpt;
use warp_core::*;

#[derive(StructOpt, Debug, Clone)]
#[structopt(
    name = "info",
    setting = structopt::clap::AppSettings::ColoredHelp,
    about = "Information about a target"
)]
pub struct InfoCommand {
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

impl InfoCommand {
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
        let (result, ()) = futures::future::join(
            warp.execute(
                &[label.clone()],
                BuildOpts {
                    concurrency_limit: self.max_workers.unwrap_or_else(num_cpus::get),
                    ..Default::default()
                },
            ),
            status_reporter.run(&[label.clone()]),
        )
        .await;

        if let Some(result) = result?.get(0) {
            println!(
                "{}",
                serde_json::to_value(result.target_manifest.as_ref().to_owned()).unwrap()
            );
            return Ok(());
        }

        Err(anyhow!("There was no target to get info on."))
    }
}
