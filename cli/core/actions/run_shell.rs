use anyhow::*;
use log::*;
use std::collections::HashMap;
use std::io::BufRead;
use std::io::BufReader;
use std::io::Write;
use std::path::PathBuf;
use std::process::{Command, Stdio};

#[derive(Debug, Clone)]
pub struct RunShellAction {
    pub env: HashMap<String, String>,
    pub script: String,
}

impl RunShellAction {
    pub fn run(self, sandbox_root: &PathBuf) -> Result<(), anyhow::Error> {
        let mut cmd = Command::new("bash");

        cmd.current_dir(sandbox_root)
            .envs(&self.env)
            .stdout(Stdio::piped())
            .args(&["-c", self.script.as_str()]);

        trace!("Running script: {:#?} {}", &self.env, &self.script);

        let output = cmd.output().context("Could not spawn bash")?;

        trace!("Got status code: {}", output.status.code().unwrap());

        /*
        BufReader::new(&*output.stdout)
            .lines()
            .filter_map(|line| line.ok())
            .for_each(|line| println!("{}", line));
        */

        if output.status.success() {
            Ok(())
        } else {
            std::io::stdout().write_all(&output.stdout).unwrap();
            std::io::stderr().write_all(&output.stderr).unwrap();
            Err(anyhow!("Error running bash script: {:?}", cmd))
        }
    }
}
