use super::*;
use crate::proto;
use crate::reporter::StatusReporter;
use dashmap::DashSet;
use std::path::PathBuf;
use structopt::StructOpt;
use warp_core::*;

#[derive(StructOpt, Debug, Clone)]
#[structopt(
    name = "lift",
    setting = structopt::clap::AppSettings::ColoredHelp,
    about = "generates signatures for this entire workspace"
)]
pub struct LiftCommand {
    #[structopt(flatten)]
    flags: Flags,
}

impl LiftCommand {
    pub async fn run(self, warp: &mut WarpEngine) -> Result<(), anyhow::Error> {
        let toolchains_registry =
            ToolchainsRegistry::fetch(&warp.workspace.paths, warp.event_channel.clone()).await?;

        let analyzers: DashSet<Label> = toolchains_registry
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
            .flat_map(|t| t.analyzer.clone())
            .collect();
        let analyzers: Vec<Label> = analyzers.into_iter().collect();

        if !analyzers.is_empty() {
            let status_reporter = StatusReporter::new(
                warp.event_channel.clone(),
                Flags {
                    show_cache_hits: false,
                    ..self.flags
                },
                Goal::Run,
            );
            let (results, ()) = futures::future::join(
                warp.execute(
                    &analyzers,
                    self.flags.into_build_opts().with_goal(Goal::Build),
                ),
                status_reporter.run(&analyzers),
            )
            .await;
            results?;

            let db = CodeDb::new(&warp.workspace).await?;

            self.run_analyzers(analyzers, warp, db).await?;
        } else {
            println!("Nothing to be done.")
        }
        Ok(())
    }

    async fn run_analyzers(
        &self,
        analyzers: Vec<Label>,
        warp: &WarpEngine,
        db: CodeDb,
    ) -> Result<(), anyhow::Error> {
        let cyan = console::Style::new().cyan().bold();

        let analyzers: Vec<String> = analyzers.iter().map(|l| l.to_string()).collect();

        let mut processes = vec![];
        let mut clients = vec![];
        for (idx, build_result) in warp
            .get_results()
            .iter()
            .filter(|br| analyzers.contains(&br.target_manifest.label.to_string()))
            .enumerate()
        {
            let port = 21000 + idx;
            let conn_str = format!("http://0.0.0.0:{}", port);

            let manifest = build_result.target_manifest.clone();
            let target = build_result.executable_target.clone();

            let process = CommandRunner::builder()
                .cwd(PathBuf::from("."))
                .manifest(manifest.clone())
                .target(target.clone())
                .stream_outputs(self.flags.experimental_stream_analyzer_outputs)
                .sandboxed(false)
                .args(vec!["start".into(), port.to_string()])
                .build()?
                .spawn()?;

            processes.push(process);

            println!(
                "{:>12} started {} on port {}",
                cyan.apply_to("Analyzer"),
                build_result.target_manifest.label.to_string(),
                port
            );

            let client = loop {
                tokio::time::sleep(std::time::Duration::from_millis(1)).await;
                let conn = proto::build::warp::codedb::analyzer_service_client::AnalyzerServiceClient::connect(conn_str.clone()).await;
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
            let request = proto::build::warp::codedb::GetInterestedPathsRequest {};
            let exts = client.get_interested_paths(request).await?.into_inner();
            extensions.extend(exts.build_files)
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
                "*/.warp*",
                "*warp-outputs*",
                "*.git*",
                "*.DS_Store*",
            ] {
                let glob = globset::Glob::new(pattern).unwrap();
                builder.add(glob);
            }
            builder.build().unwrap()
        };

        println!("{:>12} entire workspace...", cyan.apply_to("Scanning"),);

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
                    let path = path.strip_prefix(&root).unwrap().to_path_buf();
                    let request = proto::build::warp::codedb::GetProvidedSymbolsRequest {
                        file: path.to_string_lossy().to_string(),
                    };
                    let response = client.get_provided_symbols(request).await?.into_inner();
                    if !response.skipped {
                        let mut local_label: LocalLabel = path.clone().into();
                        local_label.set_workspace(&invocation_dir);
                        let label: Label = local_label.into();

                        for req in response.provides {
                            let req = req.requirement.unwrap();
                            match req {
                                proto::build::warp::requirement::Requirement::File(file) => {
                                    db.save_file(&label, &path, &hash, &file.path)
                                        .await
                                        .unwrap();
                                }
                                proto::build::warp::requirement::Requirement::Symbol(symbol) => {
                                    db.save_symbol(&label, &path, &hash, &symbol.raw, &symbol.kind)
                                        .await
                                        .unwrap();
                                }

                                proto::build::warp::requirement::Requirement::Url(_) => (),
                            }
                        }

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

        drop(processes);

        Ok(())
    }
}
