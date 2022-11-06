use crate::reporter::StatusReporter;
use std::path::PathBuf;
use structopt::StructOpt;
use tokio_util::task::LocalPoolHandle;
use warp_core::*;

pub mod analyzer {
    tonic::include_proto!("build.warp.codedb.analyzer");
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

        let lifters: Vec<Label> = toolchains_registry
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

            self.run_lifters(warp).await?;
        }
        Ok(())
    }

    async fn run_lifters(&self, warp: &WarpEngine) -> Result<(), anyhow::Error> {
        let cyan = console::Style::new().cyan().bold();

        let files = self.list_files(warp).await?;

        let root = warp.workspace.paths.workspace_root.clone();

        for (idx, build_result) in warp
            .get_results()
            .iter()
            .filter(|br| br.executable_target.rule.kind.is_runnable())
            .enumerate()
        {
            let port = 21000 + idx;

            let manifest = build_result.target_manifest.clone();
            let target = build_result.executable_target.clone();
            let invocation_dir = warp.invocation_dir.clone();

            println!(
                "{:>12} starting lifter {} on port {}",
                cyan.apply_to("Lifter"),
                build_result.target_manifest.label.to_string(),
                port
            );

            let cmd_result = CommandRunner::builder()
                .cwd(PathBuf::from("."))
                .manifest(manifest.clone())
                .target(target.clone())
                .stream_outputs(false)
                .sandboxed(false)
                .args(vec!["start".into(), port.to_string()])
                .build()?
                .spawn();

            let conn_str = format!("http://0.0.0.0:{}", port);
            let mut client =
                analyzer::analyzer_service_client::AnalyzerServiceClient::connect(conn_str).await?;

            for file in &files {
                let analyze_time = std::time::Instant::now();
                let request = analyzer::AnalyzeFileRequest {
                    file: file.to_string_lossy().to_string(),
                };
                let response = client.analyze_file(request).await?.into_inner();
                let file = file.strip_prefix(&root).unwrap().to_path_buf();
                if !response.skipped {
                    let mut local_label: LocalLabel = file.clone().into();
                    local_label.set_workspace(&invocation_dir);
                    println!(
                        "{:>12} {} ({}ms)",
                        "OK",
                        &file.to_string_lossy(),
                        analyze_time.elapsed().as_millis()
                    );
                }
            }
        }
        Ok(())
    }

    async fn list_files(&self, warp: &WarpEngine) -> Result<Vec<PathBuf>, anyhow::Error> {
        let root = warp.workspace.paths.workspace_root.clone();

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

        let mut paths = vec![];
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

                paths.push(path);
            }
        }

        Ok(paths)
    }

    async fn analyze_all_files(&self, warp: &mut WarpEngine) -> Result<(), anyhow::Error> {
        let cyan = console::Style::new().cyan().bold();
        let sources = self.list_files(warp).await?;

        let worker_pool = LocalPoolHandle::new({
            let max = num_cpus::get();
            let curr = self.max_workers.unwrap_or_else(num_cpus::get);
            curr.min(max)
        });

        for build_result in warp
            .get_results()
            .iter()
            .filter(|br| br.executable_target.rule.kind.is_runnable())
        {
            println!(
                "{:>12} using {}",
                cyan.apply_to("Analyzing"),
                build_result.target_manifest.label.to_string()
            );

            let mut tasks = vec![];
            for source_group in sources.chunks(worker_pool.num_threads()) {
                let manifest = build_result.target_manifest.clone();
                let target = build_result.executable_target.clone();
                let analyzer_label = build_result.target_manifest.label.clone();
                let invocation_dir = warp.invocation_dir.clone();

                let source_group = source_group.to_vec();

                let task = worker_pool.spawn_pinned(move || async move {
                    for source_path in source_group {
                        let analyze_time = std::time::Instant::now();

                        let cmd_result = CommandRunner::builder()
                            .cwd(PathBuf::from("."))
                            .manifest(manifest.clone())
                            .target(target.clone())
                            .stream_outputs(false)
                            .sandboxed(false)
                            .args(vec![
                                "analyze".into(),
                                source_path.to_str().unwrap().to_string(),
                            ])
                            .build()?
                            .run()
                            .await?;

                        if cmd_result.status != 0 {
                            let err = anyhow::anyhow!("Failed to analyze {}. Analyzer {} exited with status {}.\n\nStdout: {}\n\nStderr: {}",
                                source_path.to_string_lossy(),
                                analyzer_label.to_string(),
                                cmd_result.status, cmd_result.stdout, cmd_result.stderr);
                            return Err(err);
                        }

                        let resp: SourceAnalysisResponse =
                            serde_json::from_slice(cmd_result.stdout.as_bytes())?;

                        match resp {
                            SourceAnalysisResponse::UnsupportedFileType => {
                                println!(
                                    "{:>12} {} ({}ms)",
                                    "SKIP",
                                    &source_path.to_string_lossy(),
                                    analyze_time.elapsed().as_millis()
                                );
                            }

                            SourceAnalysisResponse::Analysis(source_analysis) => {
                                let mut local_label: LocalLabel = source_path.clone().into();
                                local_label.set_workspace(&invocation_dir);
                                println!(
                                    "{:>12} {} ({}ms)",
                                    "OK",
                                    &source_path.to_string_lossy(),
                                    analyze_time.elapsed().as_millis()
                                );
                            }
                        }

                        /*
                           warp.source_manager()
                           .save(&local_label, &lifted_sig.source)
                           .await?;

                           let source_file = std::mem::take(&mut lifted_sig.source);
                           let mut gen_sig: GeneratedSignature = lifted_sig.into();

                           for sig in gen_sig.signatures.iter_mut() {
                           sig.name.set_workspace(&local_label.workspace());
                           for dep in sig.deps.iter_mut().chain(sig.runtime_deps.iter_mut()) {
                           dep.set_workspace(local_label.workspace());
                           }
                           }

                           warp.signature_store()
                           .save(&local_label, &source_file, gen_sig)
                           .await?;
                           */
                    }
                    Ok(())
                });
                tasks.push(task);
            }

            for task in futures::future::join_all(tasks).await {
                task.unwrap()?;
            }
        }

        Ok(())
    }
}
