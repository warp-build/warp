use super::*;
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
    name = "shell",
    setting = structopt::clap::AppSettings::ColoredHelp,
    about = "Run a shell in this workspace"
)]
pub struct ShellCommand {
    #[structopt(flatten)]
    flags: Flags,
}

impl ShellCommand {
    pub async fn run(self, warp: &mut WarpEngine) -> Result<(), anyhow::Error> {
        let label = Label::all();

        let status_reporter = StatusReporter::new(
            warp.event_channel.clone(),
            self.flags.show_cache_hits,
            Goal::Build,
        );
        let (result, ()) = futures::future::join(
            warp.execute(
                &[label.clone()],
                self.flags.into_build_opts().with_goal(Goal::Build),
            ),
            status_reporter.run(&[label.clone()]),
        )
        .await;
        result?;

        let build_results = warp.get_results();

        let mut shell_env = HashMap::new();
        for result in &build_results {
            shell_env.extend(result.target_manifest.env_map());
        }
        shell_env.insert(
            "PROMPT".to_string(),
            format!("warp ({}) =>", &warp.workspace.name),
        );

        self.run_cmd(
            &warp.start_time,
            &warp.invocation_dir,
            label,
            PathBuf::from("/bin/zsh"),
            shell_env,
            &[],
        )
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

        let default_path = std::env::var("PATH").unwrap();
        println!("{:?}", &default_path);

        let extra_paths = env.get("PATH").cloned().unwrap_or_default();
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
