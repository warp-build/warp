use anyhow::*;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::path::PathBuf;
use std::process::Command;
use tracing::*;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ExecAction {
    pub env: HashMap<String, String>,
    pub cmd: PathBuf,
    pub args: Vec<String>,
    pub cwd: Option<PathBuf>,
    pub needs_tty: bool,
}

impl Hash for ExecAction {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.cmd.hash(state);
        self.args.hash(state);
        self.cwd.hash(state);
        self.needs_tty.hash(state);
        let mut env: Vec<(&String, &String)> = self.env.iter().collect();
        env.sort();
        env.hash(state);
    }
}

impl ExecAction {
    #[tracing::instrument(name = "action::ExecAction::run")]
    pub async fn run(&self, sandbox_root: &PathBuf) -> Result<(), anyhow::Error> {
        let mut cmd = Command::new(&self.cmd);
        cmd.envs(&self.env).args(&self.args);

        let cwd = if let Some(cwd) = &self.cwd {
            sandbox_root.join(cwd)
        } else {
            sandbox_root.to_path_buf()
        };
        cmd.current_dir(&cwd);

        debug!("Executing {:#?} in {:?}", &cmd, &cwd);

        let status = cmd
            .status()
            .context(format!("Could not spawn {:?}", self.cmd))?;

        if status.success() {
            Ok(())
        } else {
            Err(anyhow!("Error running cmd"))
        }
    }
}
