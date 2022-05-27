use anyhow::*;
use std::collections::HashMap;
use std::io::BufRead;
use std::io::BufReader;
use std::io::Write;
use std::path::PathBuf;
use std::process::{Command, Stdio};
use tracing::*;

#[derive(Debug, Clone)]
pub struct ExecAction {
    pub env: HashMap<String, String>,
    pub cmd: PathBuf,
    pub args: Vec<String>,
    pub cwd: Option<PathBuf>,
    pub needs_tty: bool,
}

impl ExecAction {
    #[tracing::instrument(name = "action::ExecAction::run")]
    pub fn run(self, sandbox_root: &PathBuf) -> Result<(), anyhow::Error> {
        let mut cmd = Command::new(&self.cmd);

        cmd.envs(&self.env);

        cmd.stdout(Stdio::piped()).args(&self.args);
        let cwd = if let Some(cwd) = self.cwd {
            sandbox_root.join(cwd)
        } else {
            sandbox_root.to_path_buf()
        };
        cmd.current_dir(&cwd);

        debug!("Executing {:#?} in {:?}", &cmd, &cwd);

        let output = cmd
            .output()
            .context(format!("Could not spawn {:?}", self.cmd))?;

        debug!("Got status code: {}", output.status.code().unwrap());

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
