use crate::reporter::*;
use std::sync::Arc;
use structopt::StructOpt;
use warp_core::*;

#[derive(StructOpt, Debug, Clone)]
#[structopt(
    name = "build",
    setting = structopt::clap::AppSettings::ColoredHelp,
    about = "Build a target in this Workspace",
)]
pub struct BuildCommand {
    #[structopt(
        help = r"The target to build.

A path to a directory with a warp file, followed by a colon
and the name of the label to be built.

Example: //my/library:lib

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

impl BuildCommand {
    pub fn all() -> BuildCommand {
        BuildCommand {
            label: "//...".to_string(),
            max_workers: None,
        }
    }

    #[tracing::instrument(name = "BuildCommand::run", skip(workspace))]
    pub async fn run(
        self,
        workspace: Workspace,
        event_channel: Arc<EventChannel>,
    ) -> Result<(), anyhow::Error> {
        let label: Label = (&workspace.aliases)
            .get(&self.label)
            .cloned()
            .unwrap_or_else(|| {
                Label::builder()
                    .with_workspace(&workspace)
                    .from_string(&self.label)
                    .unwrap()
            });

        let worker_limit = self.max_workers.unwrap_or_else(num_cpus::get);

        let warp = BuildExecutor::from_workspace(workspace, worker_limit);

        let status_reporter = StatusReporter::new(event_channel.clone());
        let (result, ()) = futures::future::join(
            warp.build(
                &[label.clone()],
                event_channel.clone(),
                BuildOpts::default(),
            ),
            status_reporter.run(&[label]),
        )
        .await;

        result?;

        Ok(())
    }
}
