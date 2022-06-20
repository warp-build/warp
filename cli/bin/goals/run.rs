use crate::reporter::*;
use anyhow::*;
use std::process::*;
use std::sync::Arc;
use structopt::StructOpt;
use tracing::*;
use zap_core::*;

#[derive(StructOpt, Debug, Clone)]
#[structopt(
    name = "run",
    setting = structopt::clap::AppSettings::ColoredHelp,
    about = "Executes a runnable target"
)]
pub struct RunGoal {
    #[structopt(help = r"The target to run.

A path to a directory with a zap file, followed by a colon
and the name of the label to be built.

Example: //my/library:shell

NOTE: not all targets are runnable. Non-runnable targets will
build their dependencies and exit.
")]
    target: String,

    #[structopt(
        help = r"The amount of workers to use to execute any necessary build tasks.",
        short = "w",
        long = "max-workers"
    )]
    max_workers: Option<usize>,

    #[structopt(name = "ARGUMENTS")]
    args: Vec<String>
}

impl RunGoal {
    pub async fn run(
        self,
        workspace: Workspace,
        event_channel: Arc<EventChannel>,
    ) -> Result<(), anyhow::Error> {
        let target: Label = self.target.into();
        debug!("Host: {}", guess_host_triple::guess_host_triple().unwrap());
        debug!("Target: {}", &target.to_string());

        if target.is_all() {
            print!(
                "You can't run everything. Please specify a target like this: zap build //my/app"
            );
            return Ok(());
        }

        let worker_limit = self.max_workers.unwrap_or(num_cpus::get());

        let local_outputs_root = workspace.paths.local_outputs_root.clone();
        let zap = BuildExecutor::from_workspace(workspace, worker_limit);

        let status_reporter = StatusReporter::new(event_channel.clone());
        let (result, ()) = futures::future::join(
            zap.build(target.clone(), event_channel.clone()),
            status_reporter.run(target.clone()),
        )
        .await;
        let computed_target = result?.unwrap();
        if let Some(run_script) = computed_target.run_script {
            let path = local_outputs_root.join(&run_script);
            let mut cmd = Command::new(path);

            cmd.stdin(Stdio::inherit())
                .stderr(Stdio::inherit())
                .stdout(Stdio::inherit())
                .args(&self.args);

            trace!("Spawning {:?}", &cmd);
            let mut proc = cmd.spawn()?;

            trace!("Waiting on {:?}", &cmd);
            proc.wait()
                .map(|_| ())
                .context(format!("Error executing {}", &target.to_string()))?;

            trace!("Exited with status: {}", cmd.status()?);
            Ok(())
        } else {
            Err(anyhow!("Target {} has no outputs!", &target.to_string()))
        }
    }
}
