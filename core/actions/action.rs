use super::*;
use anyhow::*;
use std::collections::HashMap;
use std::path::PathBuf;

#[derive(Debug, Clone)]
pub enum Action {
    Exec(ExecAction),
    Copy(CopyAction),
    WriteFile(WriteFileAction),
}

impl Action {
    pub fn write_file(contents: String, dst: PathBuf) -> Action {
        Action::WriteFile(WriteFileAction { contents, dst })
    }

    pub fn copy(src: PathBuf, dst: PathBuf) -> Action {
        Action::Copy(CopyAction { src, dst })
    }

    pub fn exec(cmd: PathBuf, args: Vec<String>, cwd: Option<PathBuf>, env: HashMap<String, String>) -> Action {
        Action::Exec(ExecAction { cmd, args, cwd, env })
    }

    pub fn run(self) -> Result<(), anyhow::Error> {
        match self {
            Action::Exec(e) => e.run(),
            Action::Copy(e) => e.run(),
            Action::WriteFile(e) => e.run(),
        }
    }
}
