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
    name = "shell",
    setting = structopt::clap::AppSettings::ColoredHelp,
    about = "Run a shell in this workspace"
)]
pub struct ShellGoal {
    #[structopt(
        help = r"The amount of workers to use to execute any necessary build tasks.",
        short = "w",
        long = "max-workers"
    )]
    max_workers: Option<usize>,
}

impl ShellGoal {
    pub async fn run(
        self,
        build_started: std::time::Instant,
        cwd: &PathBuf,
        workspace: Workspace,
        event_channel: Arc<EventChannel>,
    ) -> Result<(), anyhow::Error> {
        let label = Label::builder()
            .with_workspace(&workspace)
            .from_string("//...")
            .unwrap();

        let worker_limit = self.max_workers.unwrap_or_else(num_cpus::get);

        let warp = BuildExecutor::from_workspace(workspace.clone(), worker_limit);

        let status_reporter = StatusReporter::new(event_channel.clone());
        let (result, ()) = futures::future::join(
            warp.build(
                &[label.clone()],
                event_channel.clone(),
                BuildOpts {
                    target_filter: TargetFilter::Everything,
                    ..Default::default()
                },
            ),
            status_reporter.run(&[label.clone()]),
        )
        .await;

        result?;

        let mut shell_env = HashMap::new();
        for manifest in warp.manifests() {
            shell_env.extend(manifest.env_map());
        }
        shell_env.insert(
            "PROMPT".to_string(),
            format!("warp ({}) =>", &workspace.name),
        );

        self.run_cmd(
            build_started,
            cwd,
            label,
            PathBuf::from("/bin/zsh"),
            shell_env,
            &[],
        )
    }

    fn run_cmd(
        &self,
        build_started: std::time::Instant,
        cwd: &PathBuf,
        label: Label,
        bin: PathBuf,
        mut env: HashMap<String, String>,
        args: &[String],
    ) -> Result<(), Error> {
        let mut cmd = Command::new(bin);

        let default_path = std::env::var("PATH").unwrap();
        println!("{:?}", &default_path);

        let extra_paths = env.get("PATH").cloned().unwrap_or_else(|| "".to_string());
        env.remove("PATH");
        env.insert(
            "PATH".to_string(),
            format!("{}:{}", extra_paths, default_path),
        );

        cmd.envs(&env);

        cmd.current_dir(cwd)
            .stdin(Stdio::inherit())
            .stderr(Stdio::inherit())
            .stdout(Stdio::inherit())
            .args(args);

        debug!("Spawning {:?}", &cmd);
        let mut proc = cmd.spawn().unwrap();

        let t1 = std::time::Instant::now();
        let delta = t1.saturating_duration_since(build_started).as_millis();
        debug!("Spawned program in {:?}ms", delta);

        trace!("Waiting on {:?}", &cmd);
        proc.wait()
            .map(|_| ())
            .context(format!("Error executing {}", &label.to_string()))?;

        trace!("Exited with status: {}", cmd.status()?);

        Ok(())
    }
}
