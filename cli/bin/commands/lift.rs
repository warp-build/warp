use crate::reporter::StatusReporter;
use dashmap::DashSet;
use std::path::PathBuf;
use structopt::StructOpt;
use warp_core::*;

pub mod codedb {
    tonic::include_proto!("build.warp.codedb");
}

#[derive(StructOpt, Debug, Clone)]
#[structopt(
    name = "lift",
    setting = structopt::clap::AppSettings::ColoredHelp,
    about = "generates signatures for this entire workspace"
)]
pub struct LiftCommand {
    #[structopt(
        help = r"The amount of workers to use to execute any necessary build tasks.",
        short = "w",
        long = "max-workers"
    )]
    max_workers: Option<usize>,
}

impl LiftCommand {
    pub async fn run(self, warp: &mut WarpEngine) -> Result<(), anyhow::Error> {
        let toolchains_registry =
            ToolchainsRegistry::fetch(&warp.workspace.paths, warp.event_channel.clone()).await?;

        let lifters: DashSet<Label> = toolchains_registry
            .toolchains()
            .iter()
            .filter(|t| {
                if let Some(cfg) = warp.workspace.toolchain_configs.get(&t.id.language) {
                    match (cfg.get_string("version"), t.config.get_string("version")) {
                        (Ok(vsn0), Ok(vsn1)) => vsn0 == vsn1,
                        _ => false,
                    }
                } else {
                    false
                }
            })
            .flat_map(|t| t.lifter.clone())
            .collect();
        let lifters: Vec<Label> = lifters.into_iter().collect();

        if !lifters.is_empty() {
            let status_reporter = StatusReporter::new(warp.event_channel.clone(), false, Goal::Run);
            let (results, ()) = futures::future::join(
                warp.execute(
                    &lifters,
                    BuildOpts {
                        concurrency_limit: self.max_workers.unwrap_or_else(num_cpus::get),
                        ..BuildOpts::default()
                    },
                ),
                status_reporter.run(&lifters),
            )
            .await;
            results?;

            let db = CodeDb::new(&warp.workspace)?;

            if !db.has_key("hello-world") {
                db.write_hello()?
            }

            self.run_lifters(lifters, warp, db).await?;
        } else {
            println!("Nothing to be done.")
        }
        Ok(())
    }

    async fn run_lifters(
        &self,
        lifters: Vec<Label>,
        warp: &WarpEngine,
        db: CodeDb,
    ) -> Result<(), anyhow::Error> {
        let cyan = console::Style::new().cyan().bold();

        println!("{:>12} entire workspace...", cyan.apply_to("Scanning"),);

        let lifters: Vec<String> = lifters.iter().map(|l| l.to_string()).collect();

        let mut clients = vec![];
        for (idx, build_result) in warp
            .get_results()
            .iter()
            .filter(|br| lifters.contains(&br.target_manifest.label.to_string()))
            .enumerate()
        {
            let port = 21000 + idx;

            let manifest = build_result.target_manifest.clone();
            let target = build_result.executable_target.clone();

            let cmd_result = CommandRunner::builder()
                .cwd(PathBuf::from("."))
                .manifest(manifest.clone())
                .target(target.clone())
                .stream_outputs(false)
                .sandboxed(false)
                .args(vec!["start".into(), port.to_string()])
                .build()?
                .spawn();

            println!(
                "{:>12} started {} on port {}",
                cyan.apply_to("Lifter"),
                build_result.target_manifest.label.to_string(),
                port
            );

            let client = loop {
                tokio::time::sleep(std::time::Duration::from_millis(1)).await;
                let conn_str = format!("http://0.0.0.0:{}", port);
                let conn =
                    codedb::analyzer_service_client::AnalyzerServiceClient::connect(conn_str).await;
                if let Ok(conn) = conn {
                    break conn;
                }
            };

            clients.push(client)
        }

        if clients.is_empty() {
            return Ok(());
        }

        let root = warp.workspace.paths.workspace_root.clone();
        let invocation_dir = warp.invocation_dir.clone();

        let mut extensions = vec![];
        for client in &mut clients {
            let request = codedb::GetInterestedExtensionsRequest {};
            let exts = client
                .get_interested_extensions(request)
                .await?
                .into_inner();
            extensions.extend(exts.ext)
        }

        let match_patterns = {
            let mut builder = globset::GlobSetBuilder::new();
            for ext in &extensions {
                let glob = globset::Glob::new(&format!("*{}", ext)).unwrap();
                builder.add(glob);
            }
            builder.build().unwrap()
        };

        let skip_patterns = {
            let mut builder = globset::GlobSetBuilder::new();
            for pattern in &[
                "*target*",
                "*_build*",
                "*.warp*",
                "*warp-outputs*",
                "*.git*",
                "*.DS_Store*",
            ] {
                let glob = globset::Glob::new(pattern).unwrap();
                builder.add(glob);
            }
            builder.build().unwrap()
        };

        let mut dirs = vec![root.clone()];
        while let Some(dir) = dirs.pop() {
            let mut read_dir = tokio::fs::read_dir(&dir).await.unwrap();

            while let Ok(Some(entry)) = read_dir.next_entry().await {
                let path = entry.path().clone();

                if skip_patterns.is_match(&path) {
                    continue;
                }

                if tokio::fs::read_dir(&path).await.is_ok() {
                    dirs.push(path.clone());
                    continue;
                };

                if !match_patterns.is_match(&path) {
                    continue;
                }

                let (_contents, hash) = SourceHasher::hash_source(&path).await?;

                for client in &mut clients {
                    let analyze_time = std::time::Instant::now();
                    let request = codedb::AnalyzeFileRequest {
                        file: path
                            .strip_prefix(&root)
                            .unwrap()
                            .to_string_lossy()
                            .to_string(),
                        symbol: "".into(),
                    };
                    let response = client.analyze_file(request).await?.into_inner();
                    if !response.skipped {
                        let mut local_label: LocalLabel = path.clone().into();
                        local_label.set_workspace(&invocation_dir);

                        db.save_analysis(&path, &hash).unwrap();

                        println!(
                            "{:>12} {} ({}ms)",
                            "OK",
                            &path.to_string_lossy(),
                            analyze_time.elapsed().as_millis()
                        );
                    }
                }
            }
        }

        Ok(())
    }
}
