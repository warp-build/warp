use super::*;
use crate::reporter::*;
use structopt::StructOpt;
use warp_core::*;

#[derive(StructOpt, Debug, Clone)]
#[structopt(
    name = "get",
    setting = structopt::clap::AppSettings::ColoredHelp,
    about = "Downloads a target into this Workspace",
)]
pub struct FetchCommand {
    #[structopt(
        help = r"The target to get.

A path to a directory with a warp file, followed by a colon
and the name of the label to be built.

Example: //my/library:lib

Use //... to fetch the entire project.
",
        default_value = "//..."
    )]
    label: String,

    #[structopt(flatten)]
    flags: Flags,
}

impl FetchCommand {
    #[tracing::instrument(name = "FetchCommand::run", skip(warp))]
    pub async fn run(self, warp: &mut WarpEngine) -> Result<(), anyhow::Error> {
        let label: Label = if let Some(label) = warp.workspace.aliases.get(&self.label) {
            label.clone()
        } else {
            let mut label: Label = self.label.replace("./", "").parse()?;
            label.set_workspace(&warp.workspace.paths.workspace_root);
            label
        };

        let status_reporter =
            StatusReporter::new(warp.event_channel.clone(), self.flags, Goal::Fetch);
        let (result, ()) = futures::future::join(
            warp.execute(
                &[label.clone()],
                self.flags.into_build_opts().with_goal(Goal::Fetch),
            ),
            status_reporter.run(&[label]),
        )
        .await;

        result?;

        Ok(())
    }
}
