use anyhow::*;
use tracing::*;
use std::collections::HashMap;
use std::io::BufRead;
use std::io::BufReader;
use std::io::Write;
use std::path::PathBuf;
use std::process::{Command, Stdio};

#[derive(Debug, Clone)]
pub struct ExecAction {
    pub env: HashMap<String, String>,
    pub cmd: PathBuf,
    pub args: Vec<String>,
    pub cwd: Option<PathBuf>,
    pub needs_tty: bool,
}

impl ExecAction {
    #[tracing::instrument(name="action::ExecAction::run")]
    pub fn run(self, sandbox_root: &PathBuf) -> Result<(), anyhow::Error> {
        let mut cmd = Command::new(&self.cmd);

        cmd.envs(&self.env);

        cmd.stdout(Stdio::piped()).args(&self.args);
        if let Some(cwd) = self.cwd {
            cmd.current_dir(sandbox_root.join(cwd));
        } else {
            cmd.current_dir(sandbox_root);
        }

        trace!("Executing {:#?} {:#?}", &self.env, &cmd);

        let output = cmd
            .output()
            .context(format!("Could not spawn {:?}", self.cmd))?;

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
            std::io::stdout().write_all(&output.stdout).unwrap();
            std::io::stderr().write_all(&output.stderr).unwrap();
            Err(anyhow!("Error running {:?}", self.cmd))
        }
    }
}
