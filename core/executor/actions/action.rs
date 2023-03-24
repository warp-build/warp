use super::*;
use crate::model::ConcreteTarget;
use anyhow::*;
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, HashMap};
use std::path::PathBuf;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Action {
    Copy(CopyAction),
    Download(DownloadAction),
    SetPermissions(SetPermissionsAction),
    Exec(ExecAction),
    Extract(ExtractAction),
    RunShell(RunShellAction),
    WriteFile(WriteFileAction),
    VerifyChecksum(VerifyChecksumAction),
}

impl Action {
    pub fn run_shell(script: String, env: HashMap<String, String>, needs_tty: bool) -> Action {
        Action::RunShell(RunShellAction {
            script,
            env,
            needs_tty,
        })
    }

    pub fn write_file(contents: String, dst: PathBuf) -> Action {
        Action::WriteFile(WriteFileAction { contents, dst })
    }

    pub fn copy(src: PathBuf, dst: PathBuf) -> Action {
        Action::Copy(CopyAction { src, dst })
    }

    pub fn verify_checksum(file: PathBuf, sha1: String) -> Action {
        Action::VerifyChecksum(VerifyChecksumAction { file, sha1 })
    }

    pub fn download(url: String, sha1: String, output: PathBuf) -> Action {
        Action::Download(DownloadAction { url, sha1, output })
    }

    pub fn extract(src: PathBuf, dst: PathBuf) -> Action {
        Action::Extract(ExtractAction { src, dst })
    }

    pub fn set_permissions(file: PathBuf, executable: bool) -> Action {
        Action::SetPermissions(SetPermissionsAction { file, executable })
    }

    pub fn exec(
        cmd: PathBuf,
        args: Vec<String>,
        cwd: Option<PathBuf>,
        env: HashMap<String, String>,
        needs_tty: bool,
    ) -> Action {
        Action::Exec(ExecAction {
            cmd,
            args,
            cwd,
            env,
            needs_tty,
        })
    }

    pub async fn run(
        &self,
        target: &ConcreteTarget,
        store_root: &PathBuf,
        env: &BTreeMap<String, String>,
    ) -> Result<(), anyhow::Error> {
        match self {
            Action::Exec(a) => a.run(store_root).await,
            Action::Copy(a) => a.run(store_root).await,
            Action::Download(a) => a.run(target, store_root).await,
            Action::Extract(a) => a.run(target, store_root).await,
            Action::WriteFile(a) => a.run(store_root).await,
            Action::RunShell(a) => a.run(store_root, env).await,
            Action::SetPermissions(a) => a.run(store_root).await,
            Action::VerifyChecksum(a) => a.run(target, store_root).await,
        }
    }
}
