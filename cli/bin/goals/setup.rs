use anyhow::*;
use std::{process::Stdio, sync::Arc};
use structopt::StructOpt;
use tokio::process::Command;
use warp_core::*;

#[derive(StructOpt, Debug, Clone)]
#[structopt(
    name = "setup",
    setting = structopt::clap::AppSettings::ColoredHelp,
    about = "prepare your machine for Warp speed!"
)]
pub struct SetupGoal {}

impl SetupGoal {
    pub async fn run(
        &self,
        user: String,
        _event_channel: Arc<EventChannel>,
    ) -> Result<(), anyhow::Error> {
        println!(
            "\nWelcome {}, let's prepare your system to go to Warp speed!\n",
            user,
        );

        let setup_script = include_str!("./setup.sh");
        let mut cmd = Command::new("bash");

        cmd.stdout(Stdio::piped()).args(&["-c", setup_script]);

        let output = cmd.output().await.expect("could not run bash :(");

        if output.status.success() {
            Ok(())
        } else {
            Err(anyhow!(
                "Error preparing machine for Warp! \nStdout = {}\n\nStderr = {}",
                String::from_utf8_lossy(&output.stdout),
                String::from_utf8_lossy(&output.stderr),
            ))
        }
    }
}
