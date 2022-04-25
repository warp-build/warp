use log::*;
use std::io;
use std::io::Write;
use structopt::StructOpt;
use zap_core::*;

use std::path::PathBuf;

#[derive(StructOpt, Debug, Clone)]
#[structopt(
    name = "build",
    setting = structopt::clap::AppSettings::ColoredHelp,
    about = "Build a target in this Workspace",
)]
pub struct BuildGoal {
    #[structopt(
        help = r"The target to build.

A path to a directory with a zap file, followed by a colon
and the name of the label to be built.

Example: //my/library:lib

Use //... to build the entire project.
",
        default_value = "//..."
    )]
    target: String,
}

impl BuildGoal {
    pub fn all() -> BuildGoal {
        BuildGoal {
            target: "//...".to_string(),
        }
    }

    pub async fn run(self, workspace: Workspace) -> Result<(), anyhow::Error> {
        let target: Label = self.target.into();
        debug!("Host: {}", guess_host_triple::guess_host_triple().unwrap());
        debug!("Target: {}", &target.to_string());

        let mut zap = LocalWorker::from_workspace(workspace);

        let name = if target.is_all() {
            "workspace".to_string()
        } else {
            target.to_string()
        };

        print!("🔨 Building {}...", name);
        io::stdout().flush().unwrap();

        zap.execute(&target).await?;

        Ok(())
    }
}
