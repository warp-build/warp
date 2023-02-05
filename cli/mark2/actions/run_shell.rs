use anyhow::*;
use std::collections::BTreeMap;
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
        store_root: &PathBuf,
        env: &BTreeMap<String, String>,
    ) -> Result<(), anyhow::Error> {
        let mut cmd = Command::new("/bin/sh");

        // NOTE(@ostera): we are going to be rebuilding an "clean" environment for our shell script
        // to execute.
        //
        // This environment gets a PATH that can be overriden, but by default is limited to system
        // stuff.
        //
        let default_path = "/usr/bin:/usr/sbin:/bin:/sbin".to_string();
        let mut default_env = HashMap::new();
        default_env.insert("HOME".to_string(), "/warp/home".to_string());
        default_env.insert("PATH".to_string(), default_path);
        default_env.insert("PWD".to_string(), store_root.to_str().unwrap().to_string());
        default_env.insert("_".to_string(), "/usr/bin/env".to_string());
        for (name, value) in env.iter().chain(self.env.iter()) {
            let value = if name == "PATH" {
                let last_path = default_env.get(name).unwrap();
                format!("{}:{}", value, last_path)
            } else {
                value.to_string()
            };
            default_env.insert(name.clone(), value);
        }

        let script = self
            .script
            .replace("{{NODE_STORE_PATH}}", store_root.to_str().unwrap());

        cmd.current_dir(store_root)
            .env_clear()
            .envs(default_env)
            .stdout(Stdio::piped())
            .args(["-c", &script]);

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
                "Error running bash script: \n\nStore Root = {}\n\nScript = {}\n\nStdout = {}\n\nStderr = {}",
                store_root.to_string_lossy(),
                self.script,
                String::from_utf8_lossy(&output.stdout),
                String::from_utf8_lossy(&output.stderr),
            ))
        }
    }
}
