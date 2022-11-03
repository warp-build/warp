use crate::reporter::*;
use structopt::StructOpt;
use warp_core::*;

#[derive(StructOpt, Debug, Clone)]
#[structopt(
    name = "hash",
    setting = structopt::clap::AppSettings::ColoredHelp,
    about = "Hash a target in this Workspace",
)]
pub struct HashCommand {
    #[structopt(
        help = r"The amount of workers to use to execute any necessary hash tasks.",
        short = "w",
        long = "max-workers"
    )]
    max_workers: Option<usize>,
}

impl HashCommand {
    #[tracing::instrument(name = "HashCommand::run", skip(warp))]
    pub async fn run(self, warp: &mut WarpEngine) -> Result<(), anyhow::Error> {
        let status_reporter = StatusReporter::new(warp.event_channel.clone(), true, Goal::Build);
        let (result, ()) = futures::future::join(
            warp.execute(
                &[Label::all()],
                BuildOpts {
                    concurrency_limit: self.max_workers.unwrap_or_else(num_cpus::get),
                    experimental_file_mode: true,
                    ..Default::default()
                },
            ),
            status_reporter.run(&[Label::all()]),
        )
        .await;

        result?;

        Ok(())
    }
}
