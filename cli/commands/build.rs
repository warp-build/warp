use super::*;
use structopt::StructOpt;
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
        let mut warp = WarpDriveMarkII::new(self.flags.clone().into()).await?;
        let target = self.target.into();
        let results = warp.execute(Goal::Build, &[target]).await?;

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
