use anyhow::*;
use std::collections::HashMap;
use std::io::BufRead;
use std::io::BufReader;
use std::path::PathBuf;
use std::process::Stdio;
use tokio::process::Command;
use tracing::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RunShellAction {
    pub env: HashMap<String, String>,
    pub script: String,
    pub needs_tty: bool,
}

impl RunShellAction {
    #[tracing::instrument(name = "action::RunShellAction::run")]
    pub async fn run(
        &self,
        cache_root: &PathBuf,
        sandbox_root: &PathBuf,
    ) -> Result<(), anyhow::Error> {
        let mut cmd = Command::new("bash");

        let mut env = self.env.clone();
        env.insert(
            "WARP_BUILD_SANDBOX_NODE_PATH".to_string(),
            sandbox_root.clone().to_str().unwrap().to_string(),
        );
        env.insert(
            "WARP_BUILD_CACHE_NODE_PATH".to_string(),
            cache_root.clone().to_str().unwrap().to_string(),
        );

        cmd.current_dir(sandbox_root)
            .envs(&env)
            .stdout(Stdio::piped())
            .args(&["-c", self.script.as_str()]);

        trace!("Running script: {:#?} {}", &self.env, &self.script);

        let output = cmd.output().await.expect("could not run bash :(");

        trace!("Got status code: {}", output.status.code().unwrap());

        if self.needs_tty {
            BufReader::new(&*output.stdout)
                .lines()
                .filter_map(|line| line.ok())
                .for_each(|line| println!("{}", line));
        }

        if output.status.success() {
            Ok(())
        } else {
            Err(anyhow!(
                "Error running bash script: \n\nScript = {}\n\nStdout = {}\n\nStderr = {}",
                self.script,
                String::from_utf8_lossy(&output.stdout),
                String::from_utf8_lossy(&output.stderr),
            ))
        }
    }
}
