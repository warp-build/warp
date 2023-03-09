use super::*;
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

    #[structopt(flatten)]
    flags: Flags,
}

impl InfoCommand {
    pub async fn run(self, warp: &mut WarpEngine) -> Result<(), anyhow::Error> {
        let label: Label = if let Some(label) = warp.workspace.aliases.get(&self.label) {
            label.clone()
        } else {
            let mut label: Label = self.label.parse()?;
            label.set_workspace(&warp.workspace.paths.workspace_root);
            label
        };

        let status_reporter =
            StatusReporter::new(warp.event_channel.clone(), self.flags, Goal::Build);
        let (result, ()) = futures::future::join(
            warp.execute(
                &[label.clone()],
                self.flags.into_build_opts().with_goal(Goal::Build),
            ),
            status_reporter.run(&[label.clone()]),
        )
        .await;
        result?;

        let manifests: Vec<TargetManifest> = warp
            .get_results()
            .iter()
            .filter(|br| br.target_manifest.label.name() == label.name())
            .map(|br| (*br.target_manifest).clone())
            .collect();
        let json = serde_json::to_value(manifests)?;
        println!("{}", json);
        Ok(())
    }
}
