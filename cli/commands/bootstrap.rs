use super::*;
use crate::flags::Flags;
use structopt::StructOpt;
use warp_core::{Goal, Target, WarpDriveMarkII};

#[derive(StructOpt, Debug, Clone)]
#[structopt(
    name = "bootstrap",
    setting = structopt::clap::AppSettings::ColoredHelp,
    about = "Bootstrap a target in this Workspace",
)]
pub struct BootstrapCommand {
    #[structopt(flatten)]
    flags: Flags,
}

impl BootstrapCommand {
    pub async fn run(self) -> Result<(), anyhow::Error> {
        let mut warp = WarpDriveMarkII::new(self.flags.clone().into()).await?;

        let targets: Vec<Target> = vec![
            "./tricorders/beam/mix.exs".into(),
            "./tricorders/rust/Cargo.toml".into(),
        ];

        let results = warp.execute(Goal::Bootstrap, &targets).await?;

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
