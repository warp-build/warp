use anyhow::*;
use dialoguer::Input;
use std::collections::{BTreeMap, HashMap};
use std::path::PathBuf;
use std::process::{Command, Stdio};
use std::sync::Arc;
use structopt::StructOpt;
use tokio::fs;
use tracing::*;
use warp_core::*;

use crate::reporter::StatusReporter;

#[derive(StructOpt, Debug, Clone)]
#[structopt(
    name = "init",
    setting = structopt::clap::AppSettings::ColoredHelp,
    about = "initializes a new workspace"
)]
pub struct InitGoal {
    #[structopt(
        help = r"The amount of workers to use to execute any necessary build tasks.",
        short = "w",
        long = "max-workers"
    )]
    max_workers: Option<usize>,
}

impl InitGoal {
    pub async fn run(
        &self,
        build_started: std::time::Instant,
        cwd: &PathBuf,
        workspace: Workspace,
        event_channel: Arc<EventChannel>,
    ) -> Result<(), anyhow::Error> {
        let theme = dialoguer::theme::ColorfulTheme::default();

        println!(
            "\nWelcome {}, let's create a Workspace for your crew.\n",
            workspace.current_user,
        );

        let current_dir = fs::canonicalize(PathBuf::from("."))
            .await?
            .file_name()
            .unwrap()
            .to_str()
            .unwrap()
            .to_string();

        let workspace_name: String = Input::with_theme(&theme)
            .with_prompt("Workspace name")
            .default(current_dir)
            .show_default(true)
            .interact_text()?;

        let use_git_hooks: bool = Input::with_theme(&theme)
            .with_prompt("Should we run Warp before every commit?")
            .default("yes".to_string())
            .show_default(true)
            .interact()?
            == "yes";

        println!("\nPreparing...\n");

        let workspace_config = WorkspaceConfig::builder()
            .name(workspace_name)
            .use_git_hooks(use_git_hooks)
            .build()?;

        let workspace_file = WorkspaceFile::builder()
            .aliases({
                let mut map = BTreeMap::new();
                map.insert(
                    "erlang".to_string(),
                    "https://pkgs.warp.build/toolchains/erlang".to_string(),
                );
                map
            })
            .workspace(workspace_config)
            .toolchains({
                let mut map = BTreeMap::new();

                map.insert(
                    "cmake".to_string(),
                    FlexibleRuleConfig({
                        let mut map = BTreeMap::new();
                        map.insert(
                            "sha1".to_string(),
                            "19b1473e6ded2d234256b6aac90eb22616c5ab5e".into(),
                        );
                        map.insert("version".to_string(), "3.24.2".into());
                        map
                    }),
                );

                map.insert(
                    "erlang".to_string(),
                    FlexibleRuleConfig({
                        let mut map = BTreeMap::new();
                        map.insert(
                            "sha1".to_string(),
                            "2bb7456ccb19cf83b07506744fb540ce4d66b257".into(),
                        );
                        map.insert("version".to_string(), "25.0".into());
                        map
                    }),
                );

                map.insert(
                    "rebar3".to_string(),
                    FlexibleRuleConfig({
                        let mut map = BTreeMap::new();
                        map.insert(
                            "sha1".to_string(),
                            "143d3473dae6ea8250452f7a29db73974be51b24".into(),
                        );
                        map
                    }),
                );

                map.insert(
                    "openssl".to_string(),
                    FlexibleRuleConfig({
                        let mut map = BTreeMap::new();
                        map.insert(
                            "sha1".to_string(),
                            "73118336c58ece2e3b87f1f933f8ba446e2bdc26".into(),
                        );
                        map.insert("version".to_string(), "OpenSSL_1_1_1q".into());
                        map
                    }),
                );

                map
            })
            .remote_workspaces({
                let mut map = BTreeMap::new();

                map.insert(
                    "tools.warp.build".to_string(),
                    RemoteWorkspaceFile {
                        github: Some("warp-build/tools.warp.build".to_string()),
                        git_ref: Some("3ab02bcfce544b80f350dd3f49a9fcd35aec1491".to_string()),
                        ..RemoteWorkspaceFile::default()
                    },
                );

                map
            })
            .build()?;

        let workspace_root = PathBuf::from(".");
        workspace_file.write(&workspace_root).await?;

        let (root, workspace_file) = WorkspaceFile::find_upwards(&cwd).await?;
        std::env::set_current_dir(&root)?;

        let paths = WorkspacePaths::new(&root, None, workspace.current_user.clone())?;

        let workspace = Workspace::builder()
            .current_user(workspace.current_user)
            .paths(paths)
            .from_file(workspace_file)
            .await
            .unwrap()
            .build()?;

        let label = Label::builder()
            .with_workspace(&workspace)
            .from_string("https://tools.warp.build/erlang/lifter")
            .unwrap();
        let worker_limit = self.max_workers.unwrap_or_else(num_cpus::get);

        let local_outputs_root = workspace.paths.local_outputs_root.clone();
        let warp = BuildExecutor::from_workspace(workspace, worker_limit);

        let status_reporter = StatusReporter::new(event_channel.clone());
        let (result, ()) = futures::future::join(
            warp.build(label.clone(), event_channel.clone(), BuildOpts::default()),
            status_reporter.run(label.clone()),
        )
        .await;

        if let Some((manifest, target)) = result? {
            let mut provides_env = manifest.env_map();

            if let Some(RunScript { run_script, env }) = target.run_script {
                let path = local_outputs_root.join(&run_script);
                debug!("Running default run_script ({:?})", &path);
                provides_env.extend(env);

                for cmd in [
                    "create-manifest",
                    "create-3rdparty-buildfiles",
                    "create-buildfiles",
                ] {
                    self.run_cmd(
                        build_started,
                        cwd,
                        label.clone(),
                        path.clone(),
                        provides_env.clone(),
                        &[cmd.to_string()],
                    )?;
                }
            }
        }

        println!(
            r#"We are ready, captain!

Here's a few commands for you to try:

* Use `warp login` to set up the shared caching and remote builds
* Use `warp build` to build your workspace incrementally
* Use `warp test` to test your workspace incrementally
* Use `warp --help` to learn more about other useful commands

Build fast and prosper 🖖
"#
        );

        Ok(())
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
