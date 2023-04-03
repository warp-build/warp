use anyhow::*;
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, HashMap};
use std::hash::{Hash, Hasher};
use std::path::PathBuf;
use tokio::process::Command;
use tracing::*;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct RunShellAction {
    pub env: HashMap<String, String>,
    pub script: String,
    pub needs_tty: bool,
}

impl Hash for RunShellAction {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.script.hash(state);
        self.needs_tty.hash(state);
        let mut env: Vec<(&String, &String)> = self.env.iter().collect();
        env.sort();
        env.hash(state);
    }
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
            .envs(default_env.iter())
            .args(["-c", &script]);

        trace!("Running script: {:#?} {}", &self.env, &self.script);

        let status = cmd
            .status()
            .await
            .context(format!("Could not spawn {:?}", cmd))?;

        if status.success() {
            Ok(())
        } else {
            Err(anyhow!(
                "Error running script: \n\nStore Root = {}\n\nEnv = {:#?}\n\nScript = {}",
                store_root.to_string_lossy(),
                default_env,
                self.script,
            ))
        }
    }
}
