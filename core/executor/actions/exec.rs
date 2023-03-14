use anyhow::*;
use std::collections::HashMap;
use std::path::PathBuf;
use std::process::Command;
use tracing::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExecAction {
    pub env: HashMap<String, String>,
    pub cmd: PathBuf,
    pub args: Vec<String>,
    pub cwd: Option<PathBuf>,
    pub needs_tty: bool,
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
