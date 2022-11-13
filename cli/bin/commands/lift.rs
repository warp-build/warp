use super::*;
use crate::proto;
use crate::reporter::StatusReporter;
use dashmap::{DashMap, DashSet};
use std::{collections::BTreeMap, path::PathBuf};
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
        let mut analyzers: Vec<Label> = analyzers.into_iter().collect();
        for resolver in [
            "https://tools.warp.build/hexpm/resolver",
            "https://tools.warp.build/github/resolver",
            "https://tools.warp.build/gitlab/resolver",
        ] {
            analyzers.push(resolver.parse::<url::Url>().unwrap().into());
        }

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
        let blue_dim = console::Style::new().blue();
        let green_bold = console::Style::new().green().bold();

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

        let mut paths = vec![];
        for client in &mut clients {
            let request = proto::build::warp::codedb::GetInterestedPathsRequest {};
            let exts = client.get_interested_paths(request).await?.into_inner();
            paths.extend(exts.build_files);
            paths.extend(exts.test_files);
        }

        let match_patterns = {
            let mut builder = globset::GlobSetBuilder::new();
            for ext in &paths {
                let glob = globset::Glob::new(&format!("*{}", ext)).unwrap();
                builder.add(glob);
            }
            builder.build().unwrap()
        };

        let skip_patterns = {
            let mut builder = globset::GlobSetBuilder::new();
            for pattern in &["*target", "*_build", "*warp-outputs", "*.git", "*.DS_Store"] {
                let glob = globset::Glob::new(pattern).unwrap();
                builder.add(glob);
            }
            builder.build().unwrap()
        };

        println!("{:>12} entire workspace...", cyan.apply_to("Scanning"),);

        let deps = DashMap::new();
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
                                proto::build::warp::requirement::Requirement::Dependency(
                                    dep_req,
                                ) => {
                                    let label: Label = dep_req.name.parse().unwrap();
                                    let label = warp.label_registry().register_label(label);

                                    let resolver: Label =
                                        dep_req.signature_resolver.parse().unwrap();

                                    /* FIXME(@ostera): this is the code I wanted to write, but it won't work
                                     * because the dependency manager has temporarily inherited all
                                     * deps from all remote workspaces.
                                     *
                                     * So if we try to persist it, we get a bunch of random
                                     * dependencies that we 100% do not want.
                                     *
                                     * The real solution here is to make the
                                     * DependencyManager work with multiple workspaces.
                                     *
                                     *   let resolver =
                                     *       Some(warp.label_registry().register_label(resolver));
                                     *
                                     *   let dep = Dependency {
                                     *       label,
                                     *       resolver,
                                     *       package: dep_req.name,
                                     *       version: dep_req.version,
                                     *       url: dep_req.url.parse().unwrap(),
                                     *   };
                                     *
                                     *   warp.dependency_manager().add(dep);
                                     */

                                    let dep_json = DependencyJson::builder()
                                        .url(dep_req.url.parse()?)
                                        .resolver(Some(resolver))
                                        .version(dep_req.version.clone())
                                        .package(dep_req.name.clone())
                                        .build()
                                        .map_err(DependencyManagerError::DependencyJsonError)?;

                                    deps.insert(dep_req.url.clone(), dep_json);

                                    /* FIXME(@ostera): this is the code I wanted to write here:
                                     *
                                     *   let dep_root = warp.dependency_manager().download(dep).await?;
                                     *   dirs.push(dep_root);
                                     *
                                     * So that we can download, extract, and prepare a dependency
                                     * at this point in time rather than later.
                                     */

                                    // NOTE(@ostera): this path should really be computed within a
                                    // WorkspaceManager where we can add a DependencyWorkspace.
                                    //
                                    let final_dir = warp
                                        .workspace
                                        .paths
                                        .global_workspaces_path
                                        .join(dep_req.url.replace("://", "/"))
                                        .join(&dep_req.version);
                                    tokio::fs::create_dir_all(&final_dir).await?;

                                    let signature_file_path = final_dir.join("Warp.signature");
                                }

                                proto::build::warp::requirement::Requirement::Url(_) => (),
                            }
                        }

                        println!(
                            "{:>12} {} ({}ms)",
                            blue_dim.apply_to("OK"),
                            &path.to_string_lossy(),
                            analyze_time.elapsed().as_millis()
                        );
                    } else {
                        println!(
                            "{:>12} {} ({}ms)",
                            blue_dim.apply_to("SKIP"),
                            &path.to_string_lossy(),
                            analyze_time.elapsed().as_millis()
                        );
                    }
                }
            }
        }

        let deps: BTreeMap<String, DependencyJson> = deps.into_iter().collect();
        let dependency_file = DependencyFile::builder()
            .version("0".into())
            .dependencies(deps)
            .build()?;

        println!("{:>12} dependency manifest", cyan.apply_to("Saving"));

        dependency_file
            .write(&warp.workspace.paths.local_warp_root.join(DEPENDENCIES_JSON))
            .await?;

        drop(processes);
        println!("{:>12} lifting workspace.", green_bold.apply_to("Finished"));

        Ok(())
    }
}
