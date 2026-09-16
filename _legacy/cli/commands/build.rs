use crate::flags::Flags;
use crate::reporter::StatusReporter;
use structopt::StructOpt;
use warp_core::events::event::WorkflowEvent;
use warp_core::{Goal, WarpDriveMarkII};

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
and the name of the target to be built.

Example: ./my/library

Use @all to build the entire project.
",
        default_value = "@all"
    )]
    target: String,

    #[structopt(flatten)]
    flags: Flags,
}

impl BuildCommand {
    pub fn all() -> BuildCommand {
        BuildCommand {
            target: "@all".to_string(),
            flags: Flags::default(),
        }
    }

    pub async fn run(self) -> Result<(), anyhow::Error> {
        let config: warp_core::Config = self.flags.clone().into();
        let goal = Goal::Build;
        let target = self.target.into();
        let targets = vec![target];

        let reporter = StatusReporter::new(config.event_channel(), self.flags.clone(), goal);

        let ec = config.event_channel();
        let mut warp = WarpDriveMarkII::new(config).await?;

        let (results, _) = futures::future::join(
            async {
                let results = warp.execute(goal, &targets).await;
                ec.send(WorkflowEvent::Shutdown);
                results
            },
            reporter.run(&targets),
        )
        .await;

        let results = results?;

        if self.flags.print_hashes {
            println!();
            println!("Targets:");
            let mut results = results.get_results();
            results.sort_by(|a, b| {
                a.artifact_manifest
                    .buildstamps()
                    .build_started_at
                    .cmp(&b.artifact_manifest.buildstamps().build_started_at)
            });

            for result in results {
                let hash = result.artifact_manifest.hash();
                let target = result.artifact_manifest.target();
                println!("  {hash} => {target}");
            }
            println!();
        }

        Ok(())
    }
}
