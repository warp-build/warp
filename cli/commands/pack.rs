use crate::flags::Flags;
use crate::reporter::StatusReporter;
use structopt::StructOpt;
use warp_core::events::event::WorkflowEvent;
use warp_core::{Goal, Target, WarpDriveMarkII};

#[derive(StructOpt, Debug, Clone)]
#[structopt(
    name = "pack",
    setting = structopt::clap::AppSettings::ColoredHelp,
    about = "Packs a target into a tarball",
)]
pub struct PackCommand {
    #[structopt(help = r"The target to pack.

A path to a directory with a warp file, followed by a colon
and the name of the target to be packed.

Example: ./my/library
")]
    target: String,

    #[structopt(help = r"Upload to public store", long = "upload")]
    upload: bool,

    #[structopt(flatten)]
    flags: Flags,
}

impl PackCommand {
    pub async fn run(self) -> Result<(), anyhow::Error> {
        let config: warp_core::Config = self.flags.clone().into();
        let goal = Goal::Build;
        let target: Target = self.target.into();
        let targets = vec![target.clone()];

        let reporter = StatusReporter::new(config.event_channel(), self.flags.clone(), goal);

        let ec = config.event_channel();
        let mut warp = WarpDriveMarkII::new(config).await?;

        let (results, _) = futures::future::join(
            async {
                let _ = warp.execute(goal, &targets).await?;
                let result = warp.pack(target, self.upload).await;
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
