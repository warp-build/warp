use crate::reporter::*;
use anyhow::*;
use std::collections::HashMap;
use std::path::PathBuf;
use std::process::*;
use std::sync::Arc;
use structopt::StructOpt;
use tracing::*;
use warp_core::*;

#[derive(StructOpt, Debug, Clone)]
#[structopt(
    name = "run",
    setting = structopt::clap::AppSettings::ColoredHelp,
    about = "Executes a runnable target"
)]
pub struct RunGoal {
    #[structopt(help = r"The target to run.

A path to a directory with a warp file, followed by a colon
and the name of the label to be built.

Example: //my/library:shell

NOTE: not all targets are runnable. Non-runnable targets will
build their dependencies and exit.
")]
    label: String,

    #[structopt(
        help = r"The amount of workers to use to execute any necessary build tasks.",
        short = "w",
        long = "max-workers"
    )]
    max_workers: Option<usize>,

    #[structopt(name = "ARGUMENTS")]
    args: Vec<String>,
}

impl RunGoal {
    pub async fn run(
        self,
        cwd: &PathBuf,
        workspace: Workspace,
        event_channel: Arc<EventChannel>,
    ) -> Result<(), anyhow::Error> {
        let label: Label = (&workspace.aliases)
            .get(&self.label)
            .cloned()
            .unwrap_or_else(|| Label::from_path(&workspace.paths.workspace_root, &self.label));

        debug!("Host: {}", guess_host_triple::guess_host_triple().unwrap());
        debug!("Target: {}", &label.to_string());

        if label.is_all() {
            print!(
                "You can't run everything. Please specify a target like this: warp build //my/app"
            );
            return Ok(());
        }

        let worker_limit = self.max_workers.unwrap_or_else(num_cpus::get);

        let local_outputs_root = workspace.paths.local_outputs_root.clone();
        let warp = BuildExecutor::from_workspace(workspace, worker_limit);

        let status_reporter = StatusReporter::new(event_channel.clone());
        let (result, ()) = futures::future::join(
            warp.build(label.clone(), event_channel.clone()),
            status_reporter.run(label.clone()),
        )
        .await;

        if let Some((manifest, target)) = result? {
            for (name, path) in manifest.provides {
                if name == label.name() {
                    return self.run_cmd(cwd, label, path, HashMap::default());
                }

                if !self.args.is_empty() && name == self.args[0] {
                    return self.run_cmd(cwd, label, path, HashMap::default());
                }
            }

            if let Some(RunScript { run_script, env }) = target.run_script {
                let path = local_outputs_root.join(&run_script);
                return self.run_cmd(cwd, label, path, env);
            }
        }

        Err(anyhow!("There was no target to run."))
    }

    fn run_cmd(
        &self,
        cwd: &PathBuf,
        label: Label,
        bin: PathBuf,
        mut env: HashMap<String, String>,
    ) -> Result<(), Error> {
        let mut cmd = Command::new(bin);

        let extra_paths = env.get("PATH").cloned().unwrap_or_else(|| "".to_string());
        env.remove("PATH");
        env.insert("PATH".to_string(), format!("/bin:/usr/bin:{}", extra_paths));

        cmd.envs(&env);

        cmd.current_dir(cwd)
            .stdin(Stdio::inherit())
            .stderr(Stdio::inherit())
            .stdout(Stdio::inherit())
            .args(&self.args);

        trace!("Spawning {:?}", &cmd);
        let mut proc = cmd.spawn()?;

        trace!("Waiting on {:?}", &cmd);
        proc.wait()
            .map(|_| ())
            .context(format!("Error executing {}", &label.to_string()))?;

        trace!("Exited with status: {}", cmd.status()?);

        Ok(())
    }
}
