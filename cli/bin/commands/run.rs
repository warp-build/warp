use crate::reporter::*;
use anyhow::*;
use std::collections::HashMap;
use std::path::PathBuf;
use std::process::*;
use structopt::StructOpt;
use tracing::*;
use warp_core::*;

#[derive(StructOpt, Debug, Clone)]
#[structopt(
    name = "run",
    setting = structopt::clap::AppSettings::ColoredHelp,
    about = "Executes a runnable target"
)]
pub struct RunCommand {
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

impl RunCommand {
    pub async fn run(self, warp: &mut WarpEngine) -> Result<(), anyhow::Error> {
        let mut label: Label = if let Some(label) = (&warp.workspace.aliases).get(&self.label) {
            label.clone()
        } else {
            self.label.parse()?
        };
        label.set_workspace(&warp.workspace.paths.workspace_root);

        let status_reporter = StatusReporter::new(warp.event_channel.clone());
        let (result, ()) = futures::future::join(
            warp.execute(
                &[label.clone()],
                BuildOpts {
                    goal: Goal::Run,
                    concurrency_limit: self.max_workers.unwrap_or_else(num_cpus::get),
                    ..BuildOpts::default()
                },
            ),
            status_reporter.run(&[label.clone()]),
        )
        .await;
        result?;

        if let Some(BuildResult {
            target_manifest: manifest,
            executable_target: target,
        }) = warp.get_results().iter().find(|br| {
            br.executable_target.rule.kind.is_runnable()
                && br.target_manifest.label.name() == label.name()
        }) {
            let mut provides_env = manifest.env_map();

            if !self.args.is_empty() {
                if let Some(path) = manifest.provides.get(&self.args[0]) {
                    debug!(
                        "Found provided binary named {} at {:?}",
                        &self.args[0], &path
                    );
                    return self.run_cmd(
                        &warp.start_time,
                        &warp.invocation_dir,
                        label,
                        path.to_path_buf(),
                        provides_env,
                        &self.args[1..],
                    );
                }
            }

            if let Some(path) = manifest.provides.get(&label.name().to_string()) {
                debug!(
                    "Found provided binary named {} at {:?}",
                    label.name(),
                    &path
                );
                return self.run_cmd(
                    &warp.start_time,
                    &warp.invocation_dir,
                    label,
                    path.to_path_buf(),
                    provides_env,
                    &self.args,
                );
            }

            if let Some(RunScript { run_script, env }) = &target.run_script {
                let path = warp.workspace.paths.local_outputs_root.join(&run_script);
                debug!("Running default run_script ({:?})", &path);
                provides_env.extend(env.clone());
                return self.run_cmd(
                    &warp.start_time,
                    &warp.invocation_dir,
                    label,
                    path,
                    provides_env,
                    &self.args,
                );
            }

            if !self.args.is_empty() {
                debug!("Running command in environment");
                return self.run_cmd(
                    &warp.start_time,
                    &warp.invocation_dir,
                    label,
                    PathBuf::from(self.args[0].to_string()),
                    provides_env,
                    &self.args[1..],
                );
            }
        }

        Err(anyhow!("There was no target to run."))
    }

    fn run_cmd(
        &self,
        build_started: &std::time::Instant,
        cwd: &PathBuf,
        label: Label,
        bin: PathBuf,
        mut env: HashMap<String, String>,
        args: &[String],
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
            .args(args);

        debug!("Spawning {:?}", &cmd);
        let mut proc = cmd.spawn().unwrap();

        let t1 = std::time::Instant::now();
        let delta = t1.saturating_duration_since(*build_started).as_millis();
        debug!("Spawned program in {:?}ms", delta);

        trace!("Waiting on {:?}", &cmd);
        proc.wait()
            .map(|_| ())
            .context(format!("Error executing {}", &label.to_string()))?;

        trace!("Exited with status: {}", cmd.status()?);

        Ok(())
    }
}
