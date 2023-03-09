use anyhow::*;
use structopt::StructOpt;
use tokio::process::Command;
use warp_core::Config;

#[derive(StructOpt, Debug, Clone)]
#[structopt(
    name = "setup",
    setting = structopt::clap::AppSettings::ColoredHelp,
    about = "prepare your machine for Warp speed!"
)]
pub struct SetupCommand {}

impl SetupCommand {
    pub async fn run(&self) -> Result<(), anyhow::Error> {
        let config = warp_core::Config::builder().build()?;

        println!(
            r#"

Welcome {}, let's prepare your system for warp speed!

To do this, we need sudo permissions to:
1. Create a new Disk Volume (to keep Warp stuff in its own bubble)
2. Make sure the Warp volume mounts on start up on the right place (hint: /warp)

"#,
            config.current_user()
        );

        let setup_script = include_str!("./setup.sh");
        let mut cmd = Command::new("bash");

        cmd.args(["-c", setup_script]);

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
