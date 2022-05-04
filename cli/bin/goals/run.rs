use log::*;
use std::io::Write;
use structopt::StructOpt;
use zap_core::*;

#[derive(StructOpt, Debug, Clone)]
#[structopt(
    name = "run",
    setting = structopt::clap::AppSettings::ColoredHelp,
    about = "Executes a runnable target"
)]
pub struct RunGoal {
    #[structopt(help = r"The target to run.

A path to a directory with a zap file, followed by a colon
and the name of the label to be built.

Example: //my/library:shell

NOTE: not all targets are runnable. Non-runnable targets will
build their dependencies and exit.
")]
    target: String,
}

impl RunGoal {
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
        std::io::stdout().flush().unwrap();

        if target.is_all() {
            print!("You can't run everything. Please specify a target like this: zap build //my/app");
            return Ok(())
        }

        zap.prepare(&target).await?;
        zap.run().await?;

        Ok(())
    }
}
