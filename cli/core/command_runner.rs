use super::*;
use std::{path::PathBuf, process::Stdio};
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
}

impl From<derive_builder::UninitializedFieldError> for CommandRunnerError {
    fn from(_: derive_builder::UninitializedFieldError) -> Self {
        todo!()
    }
}

#[derive(Debug, Builder, Clone)]
#[builder(build_fn(error = "CommandRunnerError"))]
pub struct CommandRunner {
    manifest: TargetManifest,
    target: ExecutableTarget,
    cwd: PathBuf,
    args: Vec<String>,
    sandboxed: bool,
    stream_outputs: bool,
}

impl CommandRunner {
    pub fn builder() -> CommandRunnerBuilder {
        CommandRunnerBuilder::default()
    }

    pub async fn run(&self) -> Result<String, CommandRunnerError> {
        let mut shell_env = self.manifest.env_map();
        let bin = if let Some(RunScript { run_script, env }) = &self.target.run_script {
            shell_env.extend(env.clone());
            run_script
        } else {
            return Err(CommandRunnerError::NothingToRun);
        };

        let mut cmd = Command::new(bin);

        let extra_paths = shell_env
            .get("PATH")
            .cloned()
            .unwrap_or_else(|| "".to_string());
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

        String::from_utf8(output.stdout).map_err(|err| CommandRunnerError::InvalidUtf8Output {
            err,
            label: self.manifest.label.clone(),
        })
    }
}
