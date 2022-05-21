use super::*;
use anyhow::*;
use std::collections::HashMap;
use std::path::PathBuf;

#[derive(Debug, Clone)]
pub enum Action {
    Exec(ExecAction),
    Copy(CopyAction),
    WriteFile(WriteFileAction),
    RunShell(RunShellAction),
}

impl Action {
    pub fn run_shell(script: String, env: HashMap<String, String>) -> Action {
        Action::RunShell(RunShellAction { script, env })
    }

    pub fn write_file(contents: String, dst: PathBuf) -> Action {
        Action::WriteFile(WriteFileAction { contents, dst })
    }

    pub fn copy(src: PathBuf, dst: PathBuf) -> Action {
        Action::Copy(CopyAction { src, dst })
    }

    pub fn exec(cmd: PathBuf, args: Vec<String>, cwd: Option<PathBuf>, env: HashMap<String, String>) -> Action {
        Action::Exec(ExecAction { cmd, args, cwd, env })
    }

    pub fn run(self, sandbox_root: &PathBuf) -> Result<(), anyhow::Error> {
        match self {
            Action::Exec(e) => e.run(&sandbox_root),
            Action::Copy(e) => e.run(&sandbox_root),
            Action::WriteFile(e) => e.run(&sandbox_root),
            Action::RunShell(e) => e.run(&sandbox_root),
        }
    }
}
