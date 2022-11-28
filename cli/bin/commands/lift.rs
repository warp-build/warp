use super::*;
use crate::proto;
use crate::reporter::*;
use dashmap::{DashMap, DashSet};
use indicatif::ProgressBar;
use indicatif::ProgressStyle;
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
    //  scan dependency root
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
        let dependency_file = warp.workspace.paths.local_warp_root.join(DEPENDENCIES_JSON);
        let dependencies = DependencyFile::read_from_file(&dependency_file).await?;

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
                Goal::Build,
            );
            let labels = analyzers.clone();
            let (results, ()) = futures::future::join(
                warp.execute(&labels, self.flags.into_build_opts().with_goal(Goal::Build)),
                status_reporter.run(&labels),
            )
            .await;
            results?;

            let mut db = CodeDb::new(&warp.workspace).await?;

            let green_bold = console::Style::new().green().bold();

            println!("{:>12} on analyzer_services...", "Waiting");
            let mut analyzer_pool =
                AnalyzerServicePool::start(21000, analyzers.clone(), warp, self.flags).await?;

            self.install_dependencies(&mut analyzer_pool, warp, &mut db, dependencies)
                .await?;

            self.scan_workspace(&mut analyzer_pool, warp, &mut db)
                .await?;

            println!("{:>12} lifting workspace.", green_bold.apply_to("Finished"));
        } else {
            println!("Nothing to be done.")
        }
        Ok(())
    }

    async fn install_dependencies(
        &self,
        analyzer_pool: &mut AnalyzerServicePool,
        warp: &mut WarpEngine,
        db: &mut CodeDb,
        dependencies: DependencyFile,
    ) -> Result<(), anyhow::Error> {
        let workspace_root = warp.invocation_dir.clone();
        let (dep_paths, dep_labels, deps) = self
            .get_dependencies(analyzer_pool, warp, &workspace_root, &dependencies)
            .await?;

        self.update_dep_manifest(warp, &dependencies, &deps).await?;

        // NOTE(@ostera): If we found dependencies, we should fetch and build them at this point so
        // we can analyze their sources and establish the right dependency graph.
        if !dep_labels.is_empty() {
            let lifter_reporter = LifterReporter::new(
                warp.event_channel.clone(),
                Flags { ..self.flags },
                Goal::Build,
            );
            warp.prepare_for_new_run();
            let (results, ()) = futures::future::join(
                warp.execute(
                    &dep_labels,
                    self.flags.into_build_opts().with_goal(Goal::Build),
                ),
                lifter_reporter.run(&dep_labels),
            )
            .await;
            results?;

            self.scan_dep_dirs(analyzer_pool, db, dep_paths, dep_labels)
                .await?;
        }

        Ok(())
    }

    async fn update_dep_manifest(
        &self,
        warp: &WarpEngine,
        dependencies: &DependencyFile,
        deps: &DashMap<String, DependencyJson>,
    ) -> Result<(), anyhow::Error> {
        let deps: BTreeMap<String, DependencyJson> = deps
            .clone()
            .into_iter()
            .chain(dependencies.dependencies.clone().into_iter())
            .collect();
        let dependency_file = DependencyFile::builder()
            .version("0".into())
            .dependencies(deps)
            .build()?;

        dependency_file
            .write(&warp.workspace.paths.local_warp_root.join(DEPENDENCIES_JSON))
            .await?;
        Ok(())
    }

    async fn get_dependencies(
        &self,
        analyzer_pool: &mut AnalyzerServicePool,
        warp: &mut WarpEngine,
        workspace_root: &PathBuf,
        dependencies: &DependencyFile,
    ) -> Result<
        (
            DashMap<String, PathBuf>,
            Vec<Label>,
            DashMap<String, DependencyJson>,
        ),
        anyhow::Error,
    > {
        let cyan = console::Style::new().cyan().bold();

        let mut later: Vec<(PathBuf, Label)> = vec![];
        let mut queue = vec![(
            workspace_root.clone(),
            workspace_root.to_string_lossy().to_string().parse()?,
        )];
        let visited: DashSet<PathBuf> = DashSet::new();
        let pending: DashSet<Label> = DashSet::new();

        let mut dep_labels = vec![];
        let dep_paths = DashMap::new();
        let deps = DashMap::new();

        let mut include_test = true;

        let mut max_length = queue.len();

        // set up a progress bar
        let pb = {
            let style = ProgressStyle::default_bar()
                .template("{prefix:>12.cyan.bold} [{bar:25}] {pos}/{len} {wide_msg}")
                .progress_chars("=> ");
            let pb = ProgressBar::new(max_length as u64);
            pb.set_style(style);
            pb.set_prefix("Discovering");
            pb
        };

        while let Some((workspace_root, current_label)) = queue.pop().or_else(|| later.pop()) {
            max_length = max_length.max(queue.len() + later.len());
            // maximum length of the bar should be the largest number we find while queueing things
            pb.set_length(max_length as u64);
            pb.set_message(
                pending
                    .iter()
                    .map(|l| l.to_string())
                    .collect::<Vec<String>>()
                    .join(", "),
            );

            if visited.contains(&workspace_root) {
                pending.remove(&current_label);
                continue;
            }

            // NOTE(@ostera): get all dependencies from all the analyzers
            let mut current_deps = vec![];
            let mut current_paths = vec![];
            for client in &mut analyzer_pool.clients {
                let request = proto::build::warp::codedb::GetDependenciesRequest {
                    workspace_root: workspace_root.to_string_lossy().to_string(),
                    profiles: if include_test {
                        vec!["test".into()]
                    } else {
                        vec![]
                    },
                };
                // NOTE(@ostera): we only include tests the very first time (for the top-level
                // workspace)
                include_test = false;
                let response = client.get_dependencies(request).await?.into_inner();

                for dep in response.dependencies {
                    let label: Label = dep.url.parse()?;
                    warp.label_registry().register_label(label.clone());
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
                        .resolver(Some(resolver.to_hash_string()))
                        .version(dep.version.clone())
                        .package(dep.name.clone())
                        .build()
                        .map_err(DependencyManagerError::DependencyJsonError)?;

                    // NOTE(@ostera): add this dependency to the current dependency manager, so we
                    // can find its information when we're building
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

                    current_deps.push(label.clone());
                    current_paths.push(final_dir.clone());

                    queue.push((final_dir.clone(), label.clone()));
                    pending.insert(label.clone());

                    dep_labels.push(label.clone());
                    dep_paths.insert(dep.url.clone(), final_dir.clone());
                }
            }

            // NOTE(@ostera): add all dependencies to the Dependencies.json
            self.update_dep_manifest(warp, dependencies, &deps).await?;

            // NOTE(@ostera): In this case we don't have any dependencies, so there's no point in
            // trying to fetch anything. Mark as visited and move on.
            //
            if current_deps.is_empty() {
                visited.insert(workspace_root);
                pending.remove(&current_label);
                pb.inc(1);
                continue;
            }

            // NOTE(@ostera): try to fetch all the dependencies.
            //
            let download_reporter =
                DownloadReporter::new(warp.event_channel.clone(), self.flags, Goal::Fetch);
            let (results, ()) = futures::future::join(
                warp.execute(
                    &current_deps,
                    self.flags.into_build_opts().with_goal(Goal::Fetch),
                ),
                download_reporter.run(&current_deps),
            )
            .await;
            results?;

            if current_paths.iter().all(|d| visited.contains(d)) {
                visited.insert(workspace_root);
                pending.remove(&current_label);
                pb.inc(1);
            } else {
                later.push((workspace_root, current_label));
            }
        }

        pb.set_message("");
        pb.println(format!(
            "{:>12} {} dependencies",
            cyan.apply_to("Downloaded"),
            max_length
        ));

        Ok((dep_paths, dep_labels, deps))
    }

    async fn scan_dep_dirs(
        &self,
        analyzer_pool: &mut AnalyzerServicePool,
        db: &mut CodeDb,
        dep_paths: DashMap<String, PathBuf>,
        dep_labels: Vec<Label>,
    ) -> Result<(), anyhow::Error> {
        let analyze_time = std::time::Instant::now();
        let cyan = console::Style::new().cyan().bold();

        // set up a progress bar
        let pb = {
            let style = ProgressStyle::default_bar()
                .template("{prefix:>12.cyan.bold} [{bar:25}] {pos}/{len} {wide_msg}")
                .progress_chars("=> ");
            let pb = ProgressBar::new(dep_paths.len() as u64);
            pb.set_style(style);
            pb.set_prefix("Analyzing");
            pb
        };

        let mut total_files = 0;

        for (root, label) in dep_paths.iter().zip(dep_labels.iter()) {
            pb.inc(1);
            let root = &*root;

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

            let mut current_file_count = 0;

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

                    let rel_path = path.strip_prefix(&root).unwrap().to_path_buf();
                    current_file_count += 1;
                    pb.set_message(format!(
                        "{} / {}",
                        label.to_string(),
                        rel_path.to_string_lossy()
                    ));

                    let (_contents, hash) = SourceHasher::hash_source(&path).await?;

                    for client in &mut analyzer_pool.clients {
                        let request = proto::build::warp::codedb::GetProvidedSymbolsRequest {
                            file: path.to_string_lossy().to_string(),
                        };
                        let response = client.get_provided_symbols(request).await?.into_inner();
                        if !response.skipped {
                            use proto::build::warp::requirement::Requirement;
                            for req in response.provides {
                                let req = req.requirement.unwrap();
                                match req {
                                    Requirement::File(file) => {
                                        db.save_file(label, &path, &hash, &file.path)
                                            .await
                                            .unwrap();
                                    }
                                    Requirement::Symbol(symbol) => {
                                        db.save_symbol(
                                            label,
                                            &path,
                                            &hash,
                                            &symbol.raw,
                                            &symbol.kind,
                                        )
                                        .await
                                        .unwrap();
                                    }

                                    Requirement::Dependency(_) => (),
                                    Requirement::Url(_) => (),
                                }
                            }
                        }
                    }
                }
            }

            total_files += current_file_count;
        }

        pb.set_message("");
        pb.println(format!(
            "{:>12} {} sources in {} dependencies",
            cyan.apply_to("analyzed"),
            total_files,
            dep_paths.len(),
        ));

        Ok(())
    }

    async fn scan_workspace(
        &self,
        analyzer_pool: &mut AnalyzerServicePool,
        warp: &mut WarpEngine,
        db: &mut CodeDb,
    ) -> Result<(), anyhow::Error> {
        let cyan = console::Style::new().cyan().bold();

        // set up a progress bar
        let pb = {
            let style = ProgressStyle::default_bar()
                .template("{prefix:>12.cyan.bold} [{bar:25}] {pos}/{len} {wide_msg}")
                .progress_chars("=> ");
            let pb = ProgressBar::new(0);
            pb.set_style(style);
            pb.set_prefix("Analyzing");
            pb
        };

        let mut total_files = 0;

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

                let rel_path = path.strip_prefix(&root).unwrap().to_path_buf();

                total_files += 1;
                pb.set_length(total_files);
                pb.set_message(rel_path.to_string_lossy().to_string());

                let (_contents, hash) = SourceHasher::hash_source(&path).await?;

                for client in &mut analyzer_pool.clients {
                    let request = proto::build::warp::codedb::GetProvidedSymbolsRequest {
                        file: rel_path.to_string_lossy().to_string(),
                    };
                    let response = client.get_provided_symbols(request).await?.into_inner();
                    if !response.skipped {
                        let mut local_label: LocalLabel = rel_path.clone().into();
                        local_label.set_workspace(&invocation_dir);
                        let label: Label = local_label.into();

                        for req in response.provides {
                            let req = req.requirement.unwrap();
                            match req {
                                proto::build::warp::requirement::Requirement::File(file) => {
                                    db.save_file(&label, &rel_path, &hash, &file.path)
                                        .await
                                        .unwrap();
                                }
                                proto::build::warp::requirement::Requirement::Symbol(symbol) => {
                                    db.save_symbol(
                                        &label,
                                        &rel_path,
                                        &hash,
                                        &symbol.raw,
                                        &symbol.kind,
                                    )
                                    .await
                                    .unwrap();
                                }

                                proto::build::warp::requirement::Requirement::Dependency(_) => (),
                                proto::build::warp::requirement::Requirement::Url(_) => (),
                            }
                        }
                    }
                }
                pb.inc(1);
            }
        }

        pb.set_message("");
        pb.println(format!(
            "{:>12} {} sources in the current workspace",
            cyan.apply_to("Analyzed"),
            total_files,
        ));

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
        for (_idx, build_result) in warp
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

            let client = loop {
                tokio::time::sleep(std::time::Duration::from_millis(10)).await;
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
