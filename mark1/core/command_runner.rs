use super::*;
use futures::Future;
use futures::FutureExt;
use std::collections::BTreeMap;
use std::sync::Arc;
use std::{path::PathBuf, pin::Pin, process::Stdio};
use thiserror::*;
use tokio::process::Command;
use tracing::*;

#[derive(Error, Debug)]
pub enum CommandRunnerError {
    #[error("There is nothing in this target that can be executed.")]
    NothingToRun,

    #[error("Something went wrong when executing {}: {err:?}", .label.to_string())]
    ExecutionError { err: std::io::Error, label: Label },

    #[error("Something went wrong when getting the outputs of {}: {err:?}", .label.to_string())]
    OutputError { err: std::io::Error, label: Label },

    #[error("Something went wrong when reading the outputs of {} as UTF-8: {err:?}", .label.to_string())]
    InvalidUtf8Output {
        err: std::string::FromUtf8Error,
        label: Label,
    },

    #[error("Could not find run script {run_script:?} in {provides:#?}")]
    MissingRunScript {
        run_script: PathBuf,
        provides: BTreeMap<String, PathBuf>,
    },

    #[error("Could not spawn process due to: {0:?}")]
    SpawnError(tokio::task::JoinError),
}

impl From<derive_builder::UninitializedFieldError> for CommandRunnerError {
    fn from(_: derive_builder::UninitializedFieldError) -> Self {
        todo!()
    }
}

#[derive(Debug, Clone)]
pub struct CommandResult {
    pub stdout: String,
    pub stderr: String,
    pub status: i32,
}

#[derive(Debug, Builder, Clone)]
#[builder(build_fn(error = "CommandRunnerError"))]
pub struct CommandRunner {
    manifest: Arc<TargetManifest>,
    target: Arc<ExecutableTarget>,
    cwd: PathBuf,
    args: Vec<String>,
    sandboxed: bool,
    stream_outputs: bool,
}

impl CommandRunner {
    pub fn builder() -> CommandRunnerBuilder {
        CommandRunnerBuilder::default()
    }

    pub fn spawn(self) -> Result<tokio::process::Child, CommandRunnerError> {
        let mut shell_env = self.manifest.env_map();
        let bin = if let Some(RunScript { run_script, env }) = &self.target.run_script {
            shell_env.extend(env.clone());
            self.manifest
                .provides
                .iter()
                .find(|(_, path)| path.ends_with(run_script))
                .map(|(_, path)| path.to_path_buf())
                .ok_or_else(|| CommandRunnerError::MissingRunScript {
                    run_script: run_script.to_path_buf(),
                    provides: self.manifest.provides.clone(),
                })?
        } else {
            return Err(CommandRunnerError::NothingToRun);
        };

        let mut cmd = Command::new(bin);

        let extra_paths = shell_env.get("PATH").cloned().unwrap_or_default();
        shell_env.remove("PATH");
        shell_env.insert("PATH".to_string(), format!("/bin:/usr/bin:{}", extra_paths));

        let tmpdir = tempfile::tempdir().unwrap();

        let cwd = if self.sandboxed {
            tmpdir.path().to_path_buf()
        } else {
            self.cwd.clone()
        };

        cmd.envs(&shell_env).args(&self.args).current_dir(&cwd);

        if self.stream_outputs {
            cmd.stdin(Stdio::inherit())
                .stderr(Stdio::inherit())
                .stdout(Stdio::inherit());
        } else {
            cmd.stdin(Stdio::null())
                .stderr(Stdio::null())
                .stdout(Stdio::null());
        }

        Ok(cmd.kill_on_drop(true).spawn().unwrap())
    }

    #[tracing::instrument(name = "DependencyResolver::resolve", skip(self))]
    pub fn run(self) -> Pin<Box<dyn Future<Output = Result<CommandResult, CommandRunnerError>>>> {
        async move {
            let mut shell_env = self.manifest.env_map();
            let bin = if let Some(RunScript { run_script, env }) = &self.target.run_script {
                shell_env.extend(env.clone());
                self.manifest
                    .provides
                    .iter()
                    .find(|(_, path)| path.ends_with(run_script))
                    .map(|(_, path)| path.to_path_buf())
                    .ok_or_else(|| CommandRunnerError::MissingRunScript {
                        run_script: run_script.to_path_buf(),
                        provides: self.manifest.provides.clone(),
                    })?
            } else {
                return Err(CommandRunnerError::NothingToRun);
            };

            let mut cmd = Command::new(bin);

            let extra_paths = shell_env.get("PATH").cloned().unwrap_or_default();
            shell_env.remove("PATH");
            shell_env.insert("PATH".to_string(), format!("/bin:/usr/bin:{}", extra_paths));

            let tmpdir = tempfile::tempdir().unwrap();

            let cwd = if self.sandboxed {
                tmpdir.path().to_path_buf()
            } else {
                self.cwd.clone()
            };

            cmd.envs(&shell_env).args(&self.args).current_dir(&cwd);

            if self.stream_outputs {
                cmd.stdin(Stdio::inherit())
                    .stderr(Stdio::inherit())
                    .stdout(Stdio::inherit());
            } else {
                cmd.stdin(Stdio::null())
                    .stderr(Stdio::null())
                    .stdout(Stdio::null());
            }

            let mut proc = cmd.spawn().unwrap();
            proc.wait()
                .await
                .map_err(|err| CommandRunnerError::ExecutionError {
                    err,
                    label: self.manifest.label.clone(),
                })?;

            let output = cmd
                .output()
                .await
                .map_err(|err| CommandRunnerError::OutputError {
                    err,
                    label: self.manifest.label.clone(),
                })?;

            let stdout = String::from_utf8(output.stdout).map_err(|err| {
                CommandRunnerError::InvalidUtf8Output {
                    err,
                    label: self.manifest.label.clone(),
                }
            })?;

            let stderr = String::from_utf8(output.stderr).map_err(|err| {
                CommandRunnerError::InvalidUtf8Output {
                    err,
                    label: self.manifest.label.clone(),
                }
            })?;

            let status: i32 = cmd
                .status()
                .await
                .map_err(|err| CommandRunnerError::ExecutionError {
                    err,
                    label: self.manifest.label.clone(),
                })?
                .code()
                .unwrap();

            Ok(CommandResult {
                stdout,
                stderr,
                status,
            })
        }
        .boxed_local()
    }
}
