use crate::flags::Flags;
use crate::reporter::StatusReporter;
use structopt::StructOpt;
use warp_core::{events::event::WorkflowEvent, Goal, Target, WarpDriveMarkII};

#[derive(StructOpt, Debug, Clone)]
#[structopt(
    name = "fetch",
    setting = structopt::clap::AppSettings::ColoredHelp,
    about = "Fetches a target from public store without attempting to build it",
)]
pub struct FetchCommand {
    #[structopt(help = r"The target to fetch.

A path or url to a target to download

Example: https://rules.warp.build/toolchains/git
")]
    target: String,

    #[structopt(flatten)]
    flags: Flags,
}

impl FetchCommand {
    pub async fn run(self) -> Result<(), anyhow::Error> {
        let config: warp_core::Config = self.flags.clone().into();
        let target: Target = self.target.into();
        let targets = vec![target.clone()];

        let reporter = StatusReporter::new(config.event_channel(), self.flags.clone(), Goal::Fetch);

        let mut warp = WarpDriveMarkII::new(self.flags.into()).await?;
        let ec = config.event_channel();

        let (results, _) = futures::future::join(
            async {
                let result = warp.fetch_from_public_store(target).await;
                ec.send(WorkflowEvent::Shutdown);
                result
            },
            reporter.run(&targets),
        )
        .await;

        let _results = results?;

        Ok(())
    }
}
