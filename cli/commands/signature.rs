use crate::reporter::StatusReporter;

use super::*;
use anyhow::*;
use structopt::StructOpt;
use warp_core::*;

#[derive(StructOpt, Debug, Clone)]
#[structopt(
    name = "signature",
    setting = structopt::clap::AppSettings::ColoredHelp,
    about = "Display the warp signature of a target"
)]
pub struct SignatureCommand {
    #[structopt(help = r"The target to get a warp signature from.

A path to a directory with a warp file, followed by a colon
and the name of the label to be built.

Example: //my/library:shell
")]
    label: String,

    #[structopt(
        help = r"The execution goal used to generate this warp signature.",
        short = "g",
        long = "goal",
        default_value = "build"
    )]
    goal: Goal,

    #[structopt(flatten)]
    flags: Flags,
}

impl SignatureCommand {
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
                self.flags.into_build_opts().with_goal(self.goal),
            ),
            status_reporter.run(&[label.clone()]),
        )
        .await;
        result?;

        let label_id = warp.label_registry().register_label(label.clone());

        let symbol = SourceSymbol::from_label_and_goal(&label, self.goal);
        let source_file = warp
            .source_manager()
            .get_source_chunk_by_symbol(label_id, &label, &symbol)
            .await?;

        let local_label = label.get_local().unwrap();
        if let Some(sig) = warp
            .signature_store()
            .find_signature(local_label, &source_file)
            .await?
        {
            println!("{}", serde_json::to_string_pretty(&sig).unwrap());
        }

        Ok(())
    }
}
