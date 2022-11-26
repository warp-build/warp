use super::*;
use crate::proto;
use crate::reporter::*;
use dashmap::{DashMap, DashSet};
use std::{
    collections::BTreeMap,
    path::{Path, PathBuf},
};
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
    // NOTE(@ostera): the flow of this is:
    //
    // list the toolchains
    // find the analyzers
    // build everything
    // create codedb
    //
    // start analyzers
    //
    // call analyzers to get dependencies
    // build all dependencies
    //
    // get all interested paths from analyzers
    // scan workspace
    //   on interesting path
    //     hash source
    //     call analyzer
    //       get provided symbols for source
    //       save symbol in db
    //
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
            let labels = analyzers.clone();
            let (results, ()) = futures::future::join(
                warp.execute(&labels, self.flags.into_build_opts().with_goal(Goal::Build)),
                status_reporter.run(&labels),
            )
            .await;
            results?;

            let db = CodeDb::new(&warp.workspace).await?;

            self.lift_workspace(analyzers, warp, db).await?;
        } else {
            println!("Nothing to be done.")
        }
        Ok(())
    }

    async fn lift_workspace(
        &self,
        analyzers: Vec<Label>,
        mut warp: &mut WarpEngine,
        mut db: CodeDb,
    ) -> Result<(), anyhow::Error> {
        let green_bold = console::Style::new().green().bold();

        let mut analyzer_pool =
            AnalyzerServicePool::start(21000, analyzers.clone(), warp, self.flags).await?;

        self.install_dependencies(&mut analyzer_pool, &mut warp, &mut db)
            .await?;

        self.scan_workspace(&mut analyzer_pool, &mut warp, &mut db)
            .await?;

        println!("{:>12} lifting workspace.", green_bold.apply_to("Finished"));

        Ok(())
    }

    async fn install_dependencies(
        &self,
        analyzer_pool: &mut AnalyzerServicePool,
        warp: &mut WarpEngine,
        db: &mut CodeDb,
    ) -> Result<(), anyhow::Error> {
        let cyan = console::Style::new().cyan().bold();
        println!("{:>12} dependencies...", cyan.apply_to("Discovering"),);

        let mut dep_paths: Vec<PathBuf> = vec![];
        let deps = DashMap::new();
        let mut dep_labels: Vec<Label> = vec![];
        for client in &mut analyzer_pool.clients {
            let request = proto::build::warp::codedb::GetDependenciesRequest {};
            let response = client.get_dependencies(request).await?.into_inner();

            for dep in response.dependencies {
                dep_labels.push(dep.url.parse()?);
                let resolver: Label = dep.signature_resolver.parse().unwrap();

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
                 *       package: dep.name,
                 *       version: dep.version,
                 *       url: dep.url.parse().unwrap(),
                 *   };
                 *
                 *   warp.dependency_manager().add(dep);
                 */

                let dep_json = DependencyJson::builder()
                    .url(dep.url.parse()?)
                    .resolver(Some(resolver))
                    .version(dep.version.clone())
                    .package(dep.name.clone())
                    .build()
                    .map_err(DependencyManagerError::DependencyJsonError)?;

                deps.insert(dep.url.clone(), dep_json);

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
                    .join(dep.url.replace("://", "/"))
                    .join(&dep.version);

                dep_paths.push(final_dir);
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

        // NOTE(@ostera): If we found dependencies, we should fetch and build them at this point so
        // we can analyze their sources and establish the right dependency graph.
        if !dep_labels.is_empty() {
            let lifter_reporter = LifterReporter::new(
                warp.event_channel.clone(),
                Flags {
                    show_cache_hits: true,
                    ..self.flags
                },
                Goal::Build,
            );
            warp.clear_results();
            let (results, ()) = futures::future::join(
                warp.execute(
                    &dep_labels,
                    self.flags.into_build_opts().with_goal(Goal::Build),
                ),
                lifter_reporter.run(&dep_labels),
            )
            .await;
            results?;

            for (dep_root, dep_label) in dep_paths.iter().zip(dep_labels.iter()) {
                self.scan_dep_dir(analyzer_pool, warp, db, &dep_root, &dep_label)
                    .await?;
            }
        }

        Ok(())
    }

    async fn scan_dep_dir(
        &self,
        analyzer_pool: &mut AnalyzerServicePool,
        warp: &mut WarpEngine,
        db: &mut CodeDb,
        root: &Path,
        label: &Label,
    ) -> Result<(), anyhow::Error> {
        let mut paths = vec![];
        for client in &mut analyzer_pool.clients {
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

        let mut dirs = vec![root.to_path_buf()];
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

                for client in &mut analyzer_pool.clients {
                    let request = proto::build::warp::codedb::GetProvidedSymbolsRequest {
                        file: path.to_string_lossy().to_string(),
                    };
                    let path = path.strip_prefix(&root).unwrap().to_path_buf();
                    let response = client.get_provided_symbols(request).await?.into_inner();
                    if !response.skipped {
                        for req in response.provides {
                            let req = req.requirement.unwrap();
                            match req {
                                proto::build::warp::requirement::Requirement::File(file) => {
                                    db.save_file(label, &path, &hash, &file.path).await.unwrap();
                                }
                                proto::build::warp::requirement::Requirement::Symbol(symbol) => {
                                    db.save_symbol(label, &path, &hash, &symbol.raw, &symbol.kind)
                                        .await
                                        .unwrap();
                                }

                                proto::build::warp::requirement::Requirement::Dependency(_) => (),
                                proto::build::warp::requirement::Requirement::Url(_) => (),
                            }
                        }
                    }
                }
            }
        }
        Ok(())
    }

    async fn scan_workspace(
        &self,
        analyzer_pool: &mut AnalyzerServicePool,
        warp: &mut WarpEngine,
        db: &mut CodeDb,
    ) -> Result<(), anyhow::Error> {
        let cyan = console::Style::new().cyan().bold();
        let blue_dim = console::Style::new().blue();

        // NOTE(@ostera): now we can scan the workspace!
        let mut paths = vec![];
        for client in &mut analyzer_pool.clients {
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

        let root = warp.workspace.paths.workspace_root.clone();
        let invocation_dir = warp.invocation_dir.clone();
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

                for client in &mut analyzer_pool.clients {
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

                                proto::build::warp::requirement::Requirement::Dependency(_) => (),
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
        Ok(())
    }
}

pub struct AnalyzerServicePool {
    pub starting_port: i32,
    pub processes: Vec<tokio::process::Child>,
    pub clients: Vec<
        proto::build::warp::codedb::analyzer_service_client::AnalyzerServiceClient<
            tonic::transport::Channel,
        >,
    >,
    pub services: Vec<Label>,
}

impl AnalyzerServicePool {
    pub async fn start(
        starting_port: i32,
        services: Vec<Label>,
        warp: &WarpEngine,
        flags: Flags,
    ) -> Result<Self, anyhow::Error> {
        let cyan = console::Style::new().cyan().bold();

        let service_names: Vec<String> = services.iter().map(|l| l.to_string()).collect();

        let mut processes = vec![];
        let mut clients = vec![];
        for (idx, build_result) in warp
            .get_results()
            .iter()
            .filter(|br| {
                let name = br.target_manifest.label.to_string();
                service_names.contains(&name)
            })
            .enumerate()
        {
            let port = PortFinder::next().unwrap();
            let conn_str = format!("http://0.0.0.0:{}", port);

            let manifest = build_result.target_manifest.clone();
            let target = build_result.executable_target.clone();

            let process = CommandRunner::builder()
                .cwd(PathBuf::from("."))
                .manifest(manifest.clone())
                .target(target.clone())
                .stream_outputs(flags.experimental_stream_analyzer_outputs)
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

        Ok(Self {
            starting_port,
            clients,
            processes,
            services,
        })
    }
}
