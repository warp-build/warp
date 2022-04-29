use anyhow::*;
use log::*;
use std::collections::HashMap;
use std::io::Write;
use std::path::PathBuf;
use std::process::{Command, Stdio};

#[derive(Debug, Clone)]
pub struct ExecAction {
    pub env: HashMap<String, String>,
    pub cmd: PathBuf,
    pub args: Vec<String>,
    pub cwd: Option<PathBuf>,
}

impl ExecAction {
    pub fn run(self) -> Result<(), anyhow::Error> {
        let mut cmd = Command::new(&self.cmd);

        cmd.envs(&self.env);

        cmd.stdout(Stdio::piped()).args(&self.args);
        if let Some(cwd) = self.cwd {
            cmd.current_dir(cwd);
        }

        trace!("Executing {:#?} {:#?}", &self.env, &cmd);

        let output = cmd
            .output()
            .context(format!("Could not spawn {:?}", self.cmd))?;

        trace!("Got status code: {}", output.status.code().unwrap());

        if output.status.success() {
            Ok(())
        } else {
            std::io::stdout().write_all(&output.stdout).unwrap();
            std::io::stderr().write_all(&output.stderr).unwrap();
            Err(anyhow!("Error running {:?}", self.cmd))
        }
    }
}
