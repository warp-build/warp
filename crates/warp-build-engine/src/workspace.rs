use crate::action_cache::ActionCache;
use crate::error::{Result, WarpError};
use crate::fingerprint::rustc;
use crate::graph::{ActionPlan, GraphExecutor, Plan, TaskId, TaskProvider};
use crate::request::{rustc_flags, rustc_output_filename, target_environment_name};
use crate::{default_cache_directory, status_tag, BuildProfile, TargetTriple};
use serde::Deserialize;
use std::collections::{HashMap, HashSet};
use std::fs::{self, File};
use std::io::Read;
use std::num::NonZeroUsize;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;
use std::time::{Duration, Instant};
use warp_store::{BlobDigest, Cas};

static NEXT_WORK: AtomicU64 = AtomicU64::new(0);

#[derive(Clone, Debug)]
pub struct WorkspaceRequest {
    pub root: PathBuf,
    pub selection: Option<PathBuf>,
    pub output: Option<PathBuf>,
    pub cache: Option<PathBuf>,
    pub target: TargetTriple,
    pub profile: BuildProfile,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct WorkspaceReport {
    pub output: Option<PathBuf>,
    pub built: usize,
    pub cached: usize,
    pub targets: usize,
    pub elapsed: Duration,
}

pub fn build_workspace(request: &WorkspaceRequest) -> Result<WorkspaceReport> {
    let started = Instant::now();
    let result = build_workspace_inner(request, started);
    if result.is_err() {
        eprintln!(
            "{} build failed in {:.2}s",
            status_tag("FINISHED", false),
            started.elapsed().as_secs_f64()
        );
    }
    result
}

fn build_workspace_inner(request: &WorkspaceRequest, started: Instant) -> Result<WorkspaceReport> {
    let root = request
        .root
        .canonicalize()
        .map_err(|error| WarpError::io("resolve workspace root", error))?;
    let selection = request
        .selection
        .as_ref()
        .map(|path| {
            let path = if path.is_absolute() {
                path.clone()
            } else {
                root.join(path)
            };
            path.canonicalize()
                .map_err(|error| WarpError::io("resolve requested Cargo package path", error))
        })
        .transpose()?;
    let metadata = load_metadata(&root, &request.target)?;
    let root = metadata.workspace_root.clone();
    let build_directory = root
        .join("_build")
        .join(request.target.as_str())
        .join(request.profile.as_str());
    fs::create_dir_all(&build_directory)
        .map_err(|error| WarpError::io("create target/profile build directory", error))?;

    let requested_cache = match &request.cache {
        Some(path) => path.clone(),
        None => default_cache_directory()?,
    };
    let cache_root = absolute_directory(&requested_cache, "create workspace cache")?;
    if request.cache.is_some() && cache_root.starts_with(&root) {
        return Err(WarpError::new(
            "workspace cache override must be outside the workspace source tree",
        ));
    }
    if let Some(output) = &request.output {
        let output = if output.is_absolute() {
            output.clone()
        } else {
            root.join(output)
        };
        let parent = output.parent().unwrap_or(&root);
        fs::create_dir_all(parent)
            .map_err(|error| WarpError::io("create promoted binary parent", error))?;
        let parent = parent
            .canonicalize()
            .map_err(|error| WarpError::io("resolve promoted binary parent", error))?;
        if parent.starts_with(&cache_root)
            || (parent.starts_with(&root) && !parent.starts_with(&build_directory))
        {
            return Err(WarpError::new(
                "promoted binary must be outside source and cache directories, except under _build",
            ));
        }
    }
    let action_cache = Arc::new(ActionCache::new(cache_root.join("actions"))?);
    let cas = Arc::new(
        Cas::new(cache_root.join("cas"))
            .map_err(|error| WarpError::new(format!("open content store: {error}")))?,
    );

    let work = WorkDirectory::create(&cache_root)?;
    let (compiler, compiler_version) = rustc()?;
    let workspace = Workspace::from_metadata(
        metadata,
        WorkspaceBuildContext {
            root: &root,
            work_root: &work.path,
            build_directory: &build_directory,
            target_triple: request.target.clone(),
            profile: request.profile,
            compiler: &compiler,
            selection: selection.as_deref(),
        },
    )?;
    let provider = WorkspaceProvider {
        workspace: Arc::new(workspace),
        compiler,
        compiler_version,
        action_cache,
        cas,
        built: AtomicU64::new(0),
        cached: AtomicU64::new(0),
    };

    let building = provider
        .workspace
        .roots
        .iter()
        .filter_map(|task| provider.workspace.targets.get(task))
        .map(TargetBuild::label)
        .collect::<Vec<_>>();
    eprintln!("Building {}", building.join(" "));

    let workers = std::thread::available_parallelism().unwrap_or(NonZeroUsize::MIN);
    let graph = GraphExecutor::new(workers);
    let execution = graph
        .execute(&provider, provider.workspace.roots.clone())
        .map_err(|error| WarpError::new(format!("workspace graph failed: {error}")))?;

    let promoted_output = match &provider.workspace.main_binary {
        Some(task) => {
            let selected_binary = provider
                .workspace
                .targets
                .get(task)
                .expect("selected workspace binary exists");
            let output = request
                .output
                .as_ref()
                .map(|path| {
                    if path.is_absolute() {
                        path.clone()
                    } else {
                        root.join(path)
                    }
                })
                .unwrap_or_else(|| {
                    build_directory.join("bin").join(
                        selected_binary
                            .output
                            .file_name()
                            .expect("binary artifact has a filename"),
                    )
                });
            promote_binary(&selected_binary.output, &output)?;
            Some(output)
        }
        None if request.output.is_some() => {
            return Err(WarpError::new(
                "workspace has no binary target to write to the requested output path",
            ));
        }
        None => None,
    };
    let built = provider.built.load(Ordering::Relaxed) as usize;
    let cached = provider.cached.load(Ordering::Relaxed) as usize;
    Ok(WorkspaceReport {
        output: promoted_output,
        built,
        cached,
        targets: execution.completed.len(),
        elapsed: started.elapsed(),
    })
}

fn load_metadata(root: &Path, target: &TargetTriple) -> Result<CargoMetadata> {
    let output = Command::new("cargo")
        .current_dir(root)
        .args([
            "metadata",
            "--locked",
            "--format-version",
            "1",
            "--filter-platform",
        ])
        .arg(target.as_str())
        .output()
        .map_err(|error| {
            WarpError::io(
                "run cargo metadata (Cargo is required to resolve the workspace)",
                error,
            )
        })?;
    if !output.status.success() {
        return Err(WarpError::new(format!(
            "cargo metadata failed with {}\n{}{}",
            output.status,
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        )));
    }
    serde_json::from_slice(&output.stdout)
        .map_err(|error| WarpError::new(format!("parse cargo metadata output: {error}")))
}

#[derive(Deserialize)]
struct CargoMetadata {
    workspace_root: PathBuf,
    target_directory: PathBuf,
    workspace_members: Vec<String>,
    packages: Vec<CargoPackage>,
    resolve: Option<CargoResolve>,
}

#[derive(Deserialize)]
struct CargoPackage {
    id: String,
    name: String,
    version: String,
    manifest_path: PathBuf,
    edition: String,
    targets: Vec<CargoTarget>,
}

#[derive(Clone, Deserialize)]
struct CargoTarget {
    name: String,
    kind: Vec<String>,
    src_path: PathBuf,
    #[serde(default, rename = "required-features")]
    required_features: Vec<String>,
}

#[derive(Deserialize)]
struct CargoResolve {
    nodes: Vec<CargoNode>,
}

#[derive(Deserialize)]
struct CargoNode {
    id: String,
    #[serde(default)]
    features: Vec<String>,
    #[serde(default)]
    deps: Vec<CargoResolvedDependency>,
}

#[derive(Deserialize)]
struct CargoResolvedDependency {
    name: String,
    pkg: String,
    #[serde(default)]
    dep_kinds: Vec<CargoDependencyKind>,
}

#[derive(Deserialize)]
struct CargoDependencyKind {
    kind: Option<String>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum TargetKind {
    Library,
    Binary,
    ProcMacro,
}

#[derive(Deserialize)]
struct RustCompilerAction {
    program: String,
    arguments: Vec<String>,
}

const RUST_TARGET_RULE: &str = include_str!("../rules/rust_target.js");

struct TargetBuild {
    id: TaskId,
    package_id: String,
    package_name: String,
    package_version: String,
    package_root: PathBuf,
    source_root: PathBuf,
    source_fingerprint: String,
    source_file: PathBuf,
    target_name: String,
    crate_name: String,
    edition: String,
    features: Vec<String>,
    kind: TargetKind,
    dependencies: Vec<BuildDependency>,
    compiler_artifact_name: String,
    output: PathBuf,
}

impl TargetBuild {
    fn label(&self) -> String {
        format!("{}::{}", self.package_name, self.target_name)
    }
}

#[derive(Clone)]
struct BuildDependency {
    alias: String,
    package_id: String,
    internal_task: Option<TaskId>,
    artifact: Option<PathBuf>,
}

struct Workspace {
    work_directory: PathBuf,
    dependency_directories: Vec<PathBuf>,
    target_native_directories: Vec<PathBuf>,
    host_native_directories: Vec<PathBuf>,
    global_fingerprint: String,
    target: TargetTriple,
    host_target: TargetTriple,
    profile: BuildProfile,
    targets: HashMap<TaskId, TargetBuild>,
    roots: Vec<TaskId>,
    main_binary: Option<TaskId>,
}

struct WorkspaceBuildContext<'a> {
    root: &'a Path,
    work_root: &'a Path,
    build_directory: &'a Path,
    target_triple: TargetTriple,
    profile: BuildProfile,
    compiler: &'a Path,
    selection: Option<&'a Path>,
}

impl Workspace {
    fn from_metadata(metadata: CargoMetadata, context: WorkspaceBuildContext<'_>) -> Result<Self> {
        let WorkspaceBuildContext {
            root,
            work_root,
            build_directory,
            target_triple,
            profile,
            compiler,
            selection,
        } = context;
        let resolve = metadata.resolve.ok_or_else(|| {
            WarpError::new("cargo metadata did not return a resolved dependency graph")
        })?;
        let packages: HashMap<String, CargoPackage> = metadata
            .packages
            .into_iter()
            .map(|package| (package.id.clone(), package))
            .collect();
        let workspace_member_ids: HashSet<String> =
            metadata.workspace_members.into_iter().collect();
        let nodes: HashMap<String, CargoNode> = resolve
            .nodes
            .into_iter()
            .map(|node| (node.id.clone(), node))
            .collect();
        let selected_package_ids =
            select_workspace_packages(&packages, &workspace_member_ids, root, selection)?;
        let active_package_ids =
            reachable_workspace_packages(&selected_package_ids, &workspace_member_ids, &nodes);
        let host = guess_host_triple::guess_host_triple()
            .ok_or_else(|| WarpError::new("could not detect the host target triple"))?;
        let host_target = TargetTriple::parse(host)?;
        let host_profile_directory = metadata.target_directory.join(profile.as_str());
        let target_profile_directory = metadata
            .target_directory
            .join(target_triple.as_str())
            .join(profile.as_str());
        let target_profile_directories = if target_triple == host_target {
            vec![
                target_profile_directory.clone(),
                host_profile_directory.clone(),
            ]
        } else {
            vec![target_profile_directory.clone()]
        };
        let host_profile_directories = vec![host_profile_directory.clone()];

        let mut package_targets = HashMap::<String, Vec<(CargoTarget, TargetKind)>>::new();
        for package_id in &active_package_ids {
            let package = packages.get(package_id).ok_or_else(|| {
                WarpError::new(format!(
                    "Cargo metadata omitted workspace package {package_id}"
                ))
            })?;
            if package
                .targets
                .iter()
                .any(|target| target.kind.iter().any(|kind| kind == "custom-build"))
            {
                return Err(WarpError::new(format!(
                    "workspace package {} has a build script; Warp's direct rustc rules do not support build.rs yet",
                    package.name
                )));
            }
            let enabled_features: HashSet<&str> = nodes
                .get(package_id)
                .map(|node| node.features.iter().map(String::as_str).collect())
                .unwrap_or_default();
            let targets = package
                .targets
                .iter()
                .filter(|target| {
                    target
                        .required_features
                        .iter()
                        .all(|feature| enabled_features.contains(feature.as_str()))
                })
                .filter_map(|target| {
                    let kind = if target.kind.iter().any(|kind| kind == "proc-macro") {
                        Some(TargetKind::ProcMacro)
                    } else if target.kind.iter().any(|kind| kind == "lib") {
                        Some(TargetKind::Library)
                    } else if target.kind.iter().any(|kind| kind == "bin") {
                        Some(TargetKind::Binary)
                    } else {
                        None
                    }?;
                    Some((target.clone(), kind))
                })
                .collect::<Vec<_>>();
            package_targets.insert(package_id.clone(), targets);
        }

        let mut task_by_package_library = HashMap::new();
        let mut target_ids = HashMap::new();
        let mut roots = Vec::new();
        for package_id in &active_package_ids {
            for (target, kind) in &package_targets[package_id] {
                let id = task_id(package_id, &target.name)?;
                if *kind == TargetKind::Library || *kind == TargetKind::ProcMacro {
                    task_by_package_library.insert(package_id.clone(), id.clone());
                }
                target_ids.insert((package_id.clone(), target.name.clone()), id.clone());
                if selected_package_ids.contains(package_id) {
                    roots.push(id);
                }
            }
        }
        roots.sort();

        let mut external_artifacts = HashMap::<String, PathBuf>::new();
        let mut targets = HashMap::new();
        let mut external_packages = HashSet::new();
        for package_id in &active_package_ids {
            let package = packages.get(package_id).unwrap();
            let node = nodes.get(package_id).ok_or_else(|| {
                WarpError::new(format!(
                    "Cargo metadata omitted dependency information for {}",
                    package.name
                ))
            })?;
            for dependency in normal_dependencies(&node.deps) {
                if !workspace_member_ids.contains(&dependency.pkg) {
                    external_packages.insert(dependency.pkg.clone());
                }
            }
        }
        for external_id in external_packages {
            let package = packages.get(&external_id).ok_or_else(|| {
                WarpError::new(format!("Cargo metadata omitted package {external_id}"))
            })?;
            let crate_target = package
                .targets
                .iter()
                .find(|target| {
                    target
                        .kind
                        .iter()
                        .any(|kind| kind == "lib" || kind == "proc-macro")
                })
                .ok_or_else(|| {
                    WarpError::new(format!(
                        "dependency {} has no Rust library target",
                        package.name
                    ))
                })?;
            let features = nodes
                .get(&external_id)
                .map(|node| node.features.as_slice())
                .unwrap_or(&[]);
            let is_proc_macro = crate_target.kind.iter().any(|kind| kind == "proc-macro");
            let artifact_directories = if is_proc_macro {
                &host_profile_directories
            } else {
                &target_profile_directories
            };
            let artifact = find_external_artifact(ExternalArtifactRequest {
                profile_directories: artifact_directories,
                package_name: &package.name,
                version: &package.version,
                crate_name: &crate_target.name,
                proc_macro: is_proc_macro,
                features,
                target: &target_triple,
                profile,
            })?;
            external_artifacts.insert(external_id, artifact);
        }

        for package_id in &active_package_ids {
            let package = packages.get(package_id).unwrap();
            let package_root = package
                .manifest_path
                .parent()
                .ok_or_else(|| WarpError::new("Cargo manifest path has no parent"))?
                .to_path_buf();
            let package_relative = package_root
                .strip_prefix(root)
                .map_err(|_| WarpError::new("Cargo package is outside the workspace root"))?
                .to_path_buf();
            let node = nodes.get(package_id).unwrap();
            let dependencies = normal_dependencies(&node.deps)
                .into_iter()
                .map(|dependency| {
                    if workspace_member_ids.contains(&dependency.pkg) {
                        let dependency_task = task_by_package_library
                            .get(&dependency.pkg)
                            .ok_or_else(|| {
                                WarpError::new(format!(
                                    "workspace dependency {} has no library target",
                                    dependency.pkg
                                ))
                            })?;
                        Ok(BuildDependency {
                            alias: normalize_crate_name(&dependency.name),
                            package_id: dependency.pkg,
                            internal_task: Some(dependency_task.clone()),
                            artifact: None,
                        })
                    } else {
                        let artifact =
                            external_artifacts.get(&dependency.pkg).ok_or_else(|| {
                                WarpError::new(format!(
                                    "no prebuilt Cargo artifact for {}",
                                    dependency.pkg
                                ))
                            })?;
                        Ok(BuildDependency {
                            alias: normalize_crate_name(&dependency.name),
                            package_id: dependency.pkg.clone(),
                            internal_task: None,
                            artifact: Some(artifact.clone()),
                        })
                    }
                })
                .collect::<Result<Vec<_>>>()?;
            let features = node.features.clone();
            let source_fingerprint = copy_and_fingerprint_tree(
                &package_root,
                &work_root.join("sources").join(short_hash(package_id)),
            )?;
            for (target, kind) in &package_targets[package_id] {
                let id = target_ids[&(package_id.clone(), target.name.clone())].clone();
                let relative_source =
                    target.src_path.strip_prefix(&package_root).map_err(|_| {
                        WarpError::new(format!(
                            "target source {} is outside package {}",
                            target.src_path.display(),
                            package.name
                        ))
                    })?;
                let source_root = work_root.join("sources").join(short_hash(package_id));
                let effective_target = if *kind == TargetKind::ProcMacro {
                    &host_target
                } else {
                    &target_triple
                };
                let (output, compiler_artifact_name) = artifact_output(
                    *kind,
                    &target.name,
                    &package_relative,
                    build_directory,
                    effective_target,
                    compiler,
                )?;
                targets.insert(
                    id.clone(),
                    TargetBuild {
                        id,
                        package_id: package_id.clone(),
                        package_name: package.name.clone(),
                        package_version: package.version.clone(),
                        package_root: package_root.clone(),
                        source_root: source_root.clone(),
                        source_fingerprint: source_fingerprint.clone(),
                        source_file: source_root.join(relative_source),
                        target_name: target.name.clone(),
                        crate_name: normalize_crate_name(&target.name),
                        edition: package.edition.clone(),
                        features: features.clone(),
                        kind: *kind,
                        dependencies: dependencies.clone(),
                        compiler_artifact_name,
                        output,
                    },
                );
            }
        }

        let mut target_native_directories = Vec::new();
        for profile_directory in &target_profile_directories {
            target_native_directories.extend(collect_native_directories(
                &profile_directory.join("build"),
            )?);
        }
        target_native_directories.sort();
        target_native_directories.dedup();
        let mut host_native_directories = Vec::new();
        for profile_directory in &host_profile_directories {
            host_native_directories.extend(collect_native_directories(
                &profile_directory.join("build"),
            )?);
        }
        host_native_directories.sort();
        host_native_directories.dedup();
        let mut native_directories = target_native_directories
            .iter()
            .chain(&host_native_directories)
            .cloned()
            .collect::<Vec<_>>();
        native_directories.sort();
        native_directories.dedup();
        let mut global_hasher = blake3::Hasher::new();
        write_field(&mut global_hasher, RUST_TARGET_RULE.as_bytes());
        write_field(&mut global_hasher, target_triple.as_str().as_bytes());
        write_field(&mut global_hasher, profile.as_str().as_bytes());
        for path in [
            root.join("Cargo.toml"),
            root.join("Cargo.lock"),
            root.join("rust-toolchain.toml"),
            root.join(".cargo/config.toml"),
            root.join(".cargo/config"),
        ] {
            if path.is_file() {
                hash_path_into(&mut global_hasher, &path, root)?;
            }
        }
        for directory in &native_directories {
            let relative = directory
                .strip_prefix(&metadata.target_directory)
                .map_err(|_| {
                    WarpError::new("Cargo native output directory escaped the target directory")
                })?;
            write_field(
                &mut global_hasher,
                relative.to_string_lossy().replace('\\', "/").as_bytes(),
            );
            hash_tree_into(&mut global_hasher, directory)?;
        }
        for name in ["RUSTFLAGS", "CARGO_ENCODED_RUSTFLAGS", "CARGO_TARGET_DIR"] {
            if let Some(value) = std::env::var_os(name) {
                write_field(&mut global_hasher, name.as_bytes());
                write_field(&mut global_hasher, value.to_string_lossy().as_bytes());
            }
        }
        for effective_target in [&target_triple, &host_target] {
            for suffix in ["LINKER", "RUSTFLAGS"] {
                let name = target_environment_name(effective_target, suffix);
                if let Some(value) = std::env::var_os(&name) {
                    write_field(&mut global_hasher, name.as_bytes());
                    write_field(&mut global_hasher, value.to_string_lossy().as_bytes());
                }
            }
        }
        let global_fingerprint = global_hasher.finalize().to_hex().to_string();
        let mut dependency_directories = target_profile_directories
            .iter()
            .chain(host_profile_directories.iter())
            .map(|directory| directory.join("deps"))
            .collect::<Vec<_>>();
        dependency_directories.extend(
            targets
                .values()
                .filter_map(|target| target.output.parent().map(Path::to_path_buf)),
        );
        dependency_directories.sort();
        dependency_directories.dedup();
        let mut binaries = roots
            .iter()
            .filter_map(|task| targets.get(task))
            .filter(|target| target.kind == TargetKind::Binary)
            .collect::<Vec<_>>();
        binaries.sort_by(|left, right| {
            let left_preference = usize::from(left.target_name != "warp");
            let right_preference = usize::from(right.target_name != "warp");
            left_preference
                .cmp(&right_preference)
                .then(left.id.cmp(&right.id))
        });
        let main_binary = binaries.first().map(|target| target.id.clone());
        Ok(Self {
            work_directory: work_root.to_path_buf(),
            dependency_directories,
            target_native_directories,
            host_native_directories,
            global_fingerprint,
            target: target_triple,
            host_target,
            profile,
            targets,
            roots,
            main_binary,
        })
    }
}

#[derive(Clone)]
struct NormalDependency {
    name: String,
    pkg: String,
}

fn normal_dependencies(dependencies: &[CargoResolvedDependency]) -> Vec<NormalDependency> {
    dependencies
        .iter()
        .filter(|dependency| dependency.dep_kinds.iter().any(|kind| kind.kind.is_none()))
        .map(|dependency| NormalDependency {
            name: dependency.name.clone(),
            pkg: dependency.pkg.clone(),
        })
        .collect()
}

fn select_workspace_packages(
    packages: &HashMap<String, CargoPackage>,
    workspace_members: &HashSet<String>,
    workspace_root: &Path,
    selection: Option<&Path>,
) -> Result<HashSet<String>> {
    let Some(selection) = selection else {
        return Ok(workspace_members.clone());
    };
    let package_root = if selection.is_dir() {
        selection
    } else if selection
        .file_name()
        .is_some_and(|name| name == "Cargo.toml")
    {
        selection
            .parent()
            .ok_or_else(|| WarpError::new("Cargo.toml selection has no parent directory"))?
    } else {
        return Err(WarpError::new(format!(
            "build path {} must be a Cargo package directory or Cargo.toml",
            selection.display()
        )));
    };

    if package_root == workspace_root {
        return Ok(workspace_members.clone());
    }

    for package_id in workspace_members {
        let package = packages.get(package_id).ok_or_else(|| {
            WarpError::new(format!(
                "Cargo metadata omitted workspace package {package_id}"
            ))
        })?;
        let package_path = package
            .manifest_path
            .parent()
            .ok_or_else(|| WarpError::new("Cargo manifest path has no parent"))?
            .canonicalize()
            .map_err(|error| WarpError::io("resolve Cargo package directory", error))?;
        if package_path == package_root {
            return Ok(std::iter::once(package_id.clone()).collect());
        }
    }

    Err(WarpError::new(format!(
        "{} is not a Cargo package in workspace {}",
        package_root.display(),
        workspace_root.display()
    )))
}

fn reachable_workspace_packages(
    selected: &HashSet<String>,
    workspace_members: &HashSet<String>,
    nodes: &HashMap<String, CargoNode>,
) -> HashSet<String> {
    let mut reachable = selected.clone();
    let mut pending = selected.iter().cloned().collect::<Vec<_>>();
    while let Some(package_id) = pending.pop() {
        let Some(node) = nodes.get(&package_id) else {
            continue;
        };
        for dependency in normal_dependencies(&node.deps) {
            if workspace_members.contains(&dependency.pkg)
                && reachable.insert(dependency.pkg.clone())
            {
                pending.push(dependency.pkg);
            }
        }
    }
    reachable
}

struct ExternalArtifactRequest<'a> {
    profile_directories: &'a [PathBuf],
    package_name: &'a str,
    version: &'a str,
    crate_name: &'a str,
    proc_macro: bool,
    features: &'a [String],
    target: &'a TargetTriple,
    profile: BuildProfile,
}

fn find_external_artifact(request: ExternalArtifactRequest<'_>) -> Result<PathBuf> {
    let ExternalArtifactRequest {
        profile_directories,
        package_name,
        version,
        crate_name,
        proc_macro,
        features,
        target,
        profile,
    } = request;
    let extension = if proc_macro {
        if cfg!(target_os = "macos") {
            "dylib"
        } else if cfg!(windows) {
            "dll"
        } else {
            "so"
        }
    } else {
        "rlib"
    };
    let crate_name = normalize_crate_name(crate_name);
    let prefix = if proc_macro {
        format!("{}-", dynamic_library_prefix(&crate_name))
    } else {
        format!("lib{crate_name}-")
    };
    let source_marker = format!("/{package_name}-{version}/");
    let mut candidates = Vec::new();
    for profile_directory in profile_directories {
        let dependencies = profile_directory.join("deps");
        let entries = match fs::read_dir(&dependencies) {
            Ok(entries) => entries,
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => continue,
            Err(error) => return Err(WarpError::io("read Cargo dependency artifacts", error)),
        };
        for entry in entries {
            let path = entry
                .map_err(|error| WarpError::io("read Cargo dependency artifact entry", error))?
                .path();
            if path.extension().and_then(|extension| extension.to_str()) != Some(extension) {
                continue;
            }
            let Some(file_name) = path.file_name().and_then(|name| name.to_str()) else {
                continue;
            };
            if !file_name.starts_with(&prefix) {
                continue;
            }
            let Some(hash) = file_name
                .strip_prefix(&prefix)
                .and_then(|name| name.strip_suffix(&format!(".{extension}")))
            else {
                continue;
            };
            let dep_info = dependencies.join(format!("{crate_name}-{hash}.d"));
            let Ok(dep_info) = fs::read_to_string(dep_info) else {
                continue;
            };
            if !dep_info.replace('\\', "/").contains(&source_marker) {
                continue;
            }

            let fingerprint = profile_directory
                .join(".fingerprint")
                .join(format!("{crate_name}-{hash}"))
                .join(format!("lib-{crate_name}.json"));
            let feature_match = fingerprint_features(&fingerprint)
                .map(|built_features| feature_match_score(features, &built_features))
                .unwrap_or_default();
            let modified = path
                .metadata()
                .and_then(|metadata| metadata.modified())
                .unwrap_or(std::time::SystemTime::UNIX_EPOCH);
            candidates.push((feature_match, modified, path));
        }
    }

    candidates.sort_by(|left, right| left.0.cmp(&right.0).then(left.1.cmp(&right.1)));
    candidates.pop().map(|(_, _, path)| path).ok_or_else(|| {
        let cargo_profile = match profile {
            BuildProfile::Debug => "cargo build",
            BuildProfile::Release => "cargo build --release",
        };
        WarpError::new(format!(
            "no prebuilt Cargo artifact for {package_name} {version} ({crate_name}) and target {}; run `{cargo_profile} --target {}` first",
            target.as_str(),
            target.as_str()
        ))
    })
}

fn fingerprint_features(path: &Path) -> Option<Vec<String>> {
    let fingerprint: serde_json::Value = serde_json::from_slice(&fs::read(path).ok()?).ok()?;
    serde_json::from_str(fingerprint.get("features")?.as_str()?).ok()
}

fn feature_match_score(required: &[String], built: &[String]) -> usize {
    let shared = required
        .iter()
        .filter(|feature| built.contains(feature))
        .count();
    shared * 2 + usize::from(required.len() == built.len())
}

fn collect_native_directories(build_directory: &Path) -> Result<Vec<PathBuf>> {
    let mut directories = Vec::new();
    let entries = match fs::read_dir(build_directory) {
        Ok(entries) => entries,
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => return Ok(directories),
        Err(error) => return Err(WarpError::io("read Cargo build outputs", error)),
    };
    for entry in entries {
        let path = entry
            .map_err(|error| WarpError::io("read Cargo build output entry", error))?
            .path()
            .join("out");
        if path.is_dir() {
            directories.push(path);
        }
    }
    directories.sort();
    Ok(directories)
}

struct WorkspaceProvider {
    workspace: Arc<Workspace>,
    compiler: PathBuf,
    compiler_version: String,
    action_cache: Arc<ActionCache>,
    cas: Arc<Cas>,
    built: AtomicU64,
    cached: AtomicU64,
}

impl TaskProvider for WorkspaceProvider {
    fn plan(
        &self,
        task: &TaskId,
        completed_dependencies: &[TaskId],
    ) -> std::result::Result<Plan, String> {
        let target = self
            .workspace
            .targets
            .get(task)
            .ok_or_else(|| format!("unknown workspace task {task}"))?;
        let completed: HashSet<&TaskId> = completed_dependencies.iter().collect();
        let missing: Vec<TaskId> = target
            .dependencies
            .iter()
            .filter_map(|dependency| dependency.internal_task.as_ref())
            .filter(|dependency| !completed.contains(dependency))
            .cloned()
            .collect();
        if missing.is_empty() {
            Ok(Plan::Ready(ActionPlan::new(
                task.as_str(),
                completed_dependencies.to_vec(),
            )))
        } else {
            Ok(Plan::NeedsDependencies(missing))
        }
    }

    fn execute(&self, task: &TaskId, _action: &ActionPlan) -> std::result::Result<(), String> {
        let started = Instant::now();
        let label = self
            .workspace
            .targets
            .get(task)
            .map(TargetBuild::label)
            .unwrap_or_else(|| task.to_string());
        let result = self.execute_target(task);
        match &result {
            Ok(TargetOutcome::Built) => {
                eprintln!(
                    "{} {label} {:.2}s",
                    status_tag("OK", true),
                    started.elapsed().as_secs_f64()
                );
            }
            Ok(TargetOutcome::Cached) => eprintln!(
                "{} {label} {:.2}s (cached)",
                status_tag("OK", true),
                started.elapsed().as_secs_f64()
            ),
            Err(error) => {
                let detail = error.lines().next().unwrap_or(error).trim();
                eprintln!(
                    "{} {label} {:.2}s {detail}",
                    status_tag("ERR", false),
                    started.elapsed().as_secs_f64()
                );
            }
        }
        result.map(|_| ())
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum TargetOutcome {
    Built,
    Cached,
}

impl WorkspaceProvider {
    fn execute_target(&self, task: &TaskId) -> std::result::Result<TargetOutcome, String> {
        let target = self
            .workspace
            .targets
            .get(task)
            .ok_or_else(|| format!("unknown workspace task {task}"))?;
        let dependencies = self
            .resolve_dependencies(target)
            .map_err(|error| error.to_string())?;
        let key = target_action_key(
            &self.workspace,
            target,
            &dependencies,
            &self.compiler,
            &self.compiler_version,
        )
        .map_err(|error| error.to_string())?;

        if let Some(blob) = self
            .action_cache
            .lookup(&key)
            .map_err(|error| error.to_string())?
        {
            if self
                .restore(&blob, target)
                .map_err(|error| error.to_string())?
            {
                self.cached.fetch_add(1, Ordering::Relaxed);
                return Ok(TargetOutcome::Cached);
            }
        }

        let work = WorkDirectory::create(&self.workspace.work_directory.join("outputs"))
            .map_err(|error| error.to_string())?;
        let artifact = compile_target(
            &self.workspace,
            target,
            &dependencies,
            &self.compiler,
            &work.path,
        )
        .map_err(|error| error.to_string())?;
        let blob = self
            .cas
            .put_file(&artifact)
            .map_err(|error| error.to_string())?;
        self.action_cache
            .insert(&key, &blob)
            .map_err(|error| error.to_string())?;
        if !self
            .restore(&blob, target)
            .map_err(|error| error.to_string())?
        {
            return Err(format!("CAS lost compiled workspace artifact for {task}"));
        }
        self.built.fetch_add(1, Ordering::Relaxed);
        Ok(TargetOutcome::Built)
    }

    fn resolve_dependencies(&self, target: &TargetBuild) -> Result<Vec<(String, String, PathBuf)>> {
        target
            .dependencies
            .iter()
            .map(|dependency| {
                let path = if let Some(task) = &dependency.internal_task {
                    self.workspace
                        .targets
                        .get(task)
                        .map(|target| target.output.clone())
                        .ok_or_else(|| {
                            WarpError::new(format!("missing workspace dependency target {task}"))
                        })?
                } else {
                    dependency.artifact.clone().ok_or_else(|| {
                        WarpError::new(format!(
                            "missing prebuilt dependency {}",
                            dependency.package_id
                        ))
                    })?
                };
                if !path.is_file() {
                    return Err(WarpError::new(format!(
                        "dependency artifact {} was not built",
                        path.display()
                    )));
                }
                Ok((
                    dependency.alias.clone(),
                    dependency.package_id.clone(),
                    path,
                ))
            })
            .collect()
    }

    fn restore(&self, blob: &BlobDigest, target: &TargetBuild) -> Result<bool> {
        let restored = self
            .cas
            .get_file(blob, &target.output)
            .map_err(|error| WarpError::new(format!("materialize workspace artifact: {error}")))?;
        if restored && target.kind == TargetKind::Binary {
            mark_executable(&target.output)?;
        }
        Ok(restored)
    }
}

fn compile_target(
    workspace: &Workspace,
    target: &TargetBuild,
    dependencies: &[(String, String, PathBuf)],
    compiler: &Path,
    output_directory: &Path,
) -> Result<PathBuf> {
    fs::create_dir_all(output_directory)
        .map_err(|error| WarpError::io("create rustc output directory", error))?;
    let effective_target = if target.kind == TargetKind::ProcMacro {
        &workspace.host_target
    } else {
        &workspace.target
    };
    let mut compiler_arguments = vec![
        "--target".to_string(),
        effective_target.as_str().to_string(),
        "--out-dir".to_string(),
        path_argument(output_directory)?,
        "--remap-path-prefix".to_string(),
        format!("{}=/warp-source", path_argument(&target.source_root)?),
        "--remap-path-prefix".to_string(),
        format!("{}=/warp-output", path_argument(output_directory)?),
    ];
    compiler_arguments.extend(
        workspace
            .profile
            .rustc_arguments()
            .iter()
            .map(|argument| argument.to_string()),
    );
    for directory in &workspace.dependency_directories {
        compiler_arguments.push("-L".to_string());
        compiler_arguments.push(format!("dependency={}", path_argument(directory)?));
    }
    let native_directories = if target.kind == TargetKind::ProcMacro {
        &workspace.host_native_directories
    } else {
        &workspace.target_native_directories
    };
    for directory in native_directories {
        compiler_arguments.push("-L".to_string());
        compiler_arguments.push(format!("native={}", path_argument(directory)?));
    }
    compiler_arguments.extend(rustc_flags(effective_target));

    let rule_configuration = serde_json::json!({
        "crate_name": target.crate_name,
        "crate_type": match target.kind {
            TargetKind::Library => "rlib",
            TargetKind::Binary => "bin",
            TargetKind::ProcMacro => "proc-macro",
        },
        "edition": target.edition,
        "compiler_arguments": compiler_arguments,
        "features": target.features,
        "dependencies": dependencies.iter().map(|(alias, _, path)| {
            serde_json::json!({ "name": alias, "path": path.to_string_lossy() })
        }).collect::<Vec<_>>(),
        "source": target.source_file,
    });
    let rule_source = format!(
        "const configuration = {rule_configuration};\n{}",
        RUST_TARGET_RULE
    );
    let action = warp_build_language::evaluate(&rule_source)
        .map_err(|error| WarpError::new(format!("evaluate Rust build rule: {error}")))?;
    let action: RustCompilerAction = serde_json::from_value(action)
        .map_err(|error| WarpError::new(format!("parse Rust build rule action: {error}")))?;
    if action.program != "rustc" {
        return Err(WarpError::new(format!(
            "Rust build rule requested unsupported program {:?}",
            action.program
        )));
    }

    let mut command = Command::new(compiler);
    if target.kind != TargetKind::ProcMacro {
        add_host_native_loader_paths(&mut command, &workspace.host_native_directories)?;
    }
    command
        .current_dir(&target.source_root)
        .args(action.arguments)
        .env("CARGO_MANIFEST_DIR", &target.package_root)
        .env("CARGO_PKG_NAME", &target.package_name)
        .env("CARGO_PKG_VERSION", &target.package_version);

    let output = command
        .output()
        .map_err(|error| WarpError::io("run rustc for workspace target", error))?;
    if !output.status.success() {
        return Err(WarpError::new(format!(
            "rustc failed for {} with {}\n{}{}",
            target.id,
            output.status,
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        )));
    }

    let artifact = output_directory.join(&target.compiler_artifact_name);
    if !artifact.is_file() {
        return Err(WarpError::new(format!(
            "rustc succeeded but did not produce expected artifact {}",
            artifact.display()
        )));
    }
    Ok(artifact)
}

fn add_host_native_loader_paths(command: &mut Command, directories: &[PathBuf]) -> Result<()> {
    if directories.is_empty() {
        return Ok(());
    }
    let variable = if cfg!(target_os = "macos") {
        "DYLD_LIBRARY_PATH"
    } else if cfg!(windows) {
        "PATH"
    } else {
        "LD_LIBRARY_PATH"
    };
    let mut paths = directories.to_vec();
    if let Some(existing) = std::env::var_os(variable) {
        paths.extend(std::env::split_paths(&existing));
    }
    let joined = std::env::join_paths(paths)
        .map_err(|error| WarpError::new(format!("compose {variable} for host tools: {error}")))?;
    command.env(variable, joined);
    Ok(())
}

fn target_action_key(
    workspace: &Workspace,
    target: &TargetBuild,
    dependencies: &[(String, String, PathBuf)],
    compiler: &Path,
    compiler_version: &str,
) -> Result<String> {
    let mut hasher = blake3::Hasher::new();
    write_field(&mut hasher, b"warp-cargo-package-action-v1");
    write_field(&mut hasher, workspace.global_fingerprint.as_bytes());
    let effective_target = if target.kind == TargetKind::ProcMacro {
        &workspace.host_target
    } else {
        &workspace.target
    };
    write_field(&mut hasher, effective_target.as_str().as_bytes());
    write_field(&mut hasher, workspace.profile.as_str().as_bytes());
    for argument in rustc_flags(effective_target) {
        write_field(&mut hasher, argument.as_bytes());
    }
    write_field(&mut hasher, target.package_id.as_bytes());
    write_field(&mut hasher, target.package_version.as_bytes());
    write_field(&mut hasher, target.crate_name.as_bytes());
    write_field(&mut hasher, target.edition.as_bytes());
    write_field(&mut hasher, compiler.to_string_lossy().as_bytes());
    write_field(&mut hasher, compiler_version.as_bytes());
    write_field(&mut hasher, target.source_fingerprint.as_bytes());
    let source_relative = target
        .source_file
        .strip_prefix(&target.source_root)
        .map_err(|_| WarpError::new("target source escaped its package snapshot"))?;
    write_field(&mut hasher, source_relative.to_string_lossy().as_bytes());
    write_field(
        &mut hasher,
        match target.kind {
            TargetKind::Library => b"library",
            TargetKind::Binary => b"binary",
            TargetKind::ProcMacro => b"proc-macro",
        },
    );
    for feature in &target.features {
        write_field(&mut hasher, feature.as_bytes());
    }
    let mut dependencies = dependencies.to_vec();
    dependencies.sort_by(|left, right| left.0.cmp(&right.0));
    for (alias, package_id, path) in dependencies {
        write_field(&mut hasher, alias.as_bytes());
        write_field(&mut hasher, package_id.as_bytes());
        let digest = hash_file(&path)?;
        write_field(&mut hasher, digest.as_bytes());
    }
    Ok(format!("blake3:{}", hasher.finalize().to_hex()))
}

fn copy_and_fingerprint_tree(source: &Path, destination: &Path) -> Result<String> {
    fs::create_dir_all(destination)
        .map_err(|error| WarpError::io("create workspace source snapshot", error))?;
    let mut files = Vec::new();
    collect_files(source, source, &mut files)?;
    files.sort();
    let mut hasher = blake3::Hasher::new();
    for relative in files {
        let source_path = source.join(&relative);
        let destination_path = destination.join(&relative);
        if let Some(parent) = destination_path.parent() {
            fs::create_dir_all(parent)
                .map_err(|error| WarpError::io("create package snapshot directory", error))?;
        }
        fs::copy(&source_path, &destination_path)
            .map_err(|error| WarpError::io("copy package source into immutable snapshot", error))?;
        write_field(
            &mut hasher,
            relative.to_string_lossy().replace('\\', "/").as_bytes(),
        );
        hash_path_into(&mut hasher, &destination_path, destination)?;
    }
    Ok(hasher.finalize().to_hex().to_string())
}

fn collect_files(root: &Path, directory: &Path, files: &mut Vec<PathBuf>) -> Result<()> {
    let entries = fs::read_dir(directory)
        .map_err(|error| WarpError::io("scan workspace package sources", error))?;
    for entry in entries {
        let path = entry
            .map_err(|error| WarpError::io("read workspace source directory entry", error))?
            .path();
        let kind = fs::symlink_metadata(&path)
            .map_err(|error| WarpError::io("inspect workspace source path", error))?
            .file_type();
        if kind.is_dir()
            && path
                .file_name()
                .is_some_and(|name| matches!(name.to_str(), Some(".git" | "target" | "_build")))
        {
            continue;
        }
        if kind.is_symlink() {
            return Err(WarpError::new(format!(
                "workspace package contains a symbolic link that Warp will not follow: {}",
                path.display()
            )));
        }
        if kind.is_dir() {
            collect_files(root, &path, files)?;
        } else if kind.is_file() {
            files.push(
                path.strip_prefix(root)
                    .map_err(|_| WarpError::new("workspace source escaped its package root"))?
                    .to_path_buf(),
            );
        }
    }
    Ok(())
}

fn hash_tree_into(hasher: &mut blake3::Hasher, root: &Path) -> Result<()> {
    let mut files = Vec::new();
    collect_files(root, root, &mut files)?;
    files.sort();
    for relative in files {
        hash_path_into(hasher, &root.join(relative), root)?;
    }
    Ok(())
}

fn hash_path_into(hasher: &mut blake3::Hasher, path: &Path, root: &Path) -> Result<()> {
    let relative = path
        .strip_prefix(root)
        .map_err(|_| WarpError::new("workspace fingerprint input escaped its root"))?;
    write_field(
        hasher,
        relative.to_string_lossy().replace('\\', "/").as_bytes(),
    );
    let mut file = File::open(path)
        .map_err(|error| WarpError::io("read workspace fingerprint input", error))?;
    let length = file
        .metadata()
        .map_err(|error| WarpError::io("inspect workspace fingerprint input", error))?
        .len()
        .to_le_bytes();
    write_field(hasher, &length);
    let mut buffer = [0; 32 * 1024];
    loop {
        let count = file
            .read(&mut buffer)
            .map_err(|error| WarpError::io("read workspace fingerprint input", error))?;
        if count == 0 {
            return Ok(());
        }
        hasher.update(&buffer[..count]);
    }
}

fn path_argument(path: &Path) -> Result<String> {
    path.to_str()
        .map(str::to_owned)
        .ok_or_else(|| WarpError::new(format!("rustc path is not valid UTF-8: {}", path.display())))
}

fn hash_file(path: &Path) -> Result<String> {
    let mut file =
        File::open(path).map_err(|error| WarpError::io("read dependency artifact", error))?;
    let mut hasher = blake3::Hasher::new();
    let mut buffer = [0; 64 * 1024];
    loop {
        let count = file
            .read(&mut buffer)
            .map_err(|error| WarpError::io("hash dependency artifact", error))?;
        if count == 0 {
            return Ok(hasher.finalize().to_hex().to_string());
        }
        hasher.update(&buffer[..count]);
    }
}

fn write_field(hasher: &mut blake3::Hasher, value: &[u8]) {
    hasher.update(&(value.len() as u64).to_le_bytes());
    hasher.update(value);
}

fn absolute_directory(path: &Path, action: &str) -> Result<PathBuf> {
    fs::create_dir_all(path).map_err(|error| WarpError::io(action, error))?;
    path.canonicalize()
        .map_err(|error| WarpError::io("resolve workspace cache directory", error))
}

fn artifact_output(
    kind: TargetKind,
    target_name: &str,
    package_relative: &Path,
    build_directory: &Path,
    target: &TargetTriple,
    compiler: &Path,
) -> Result<(PathBuf, String)> {
    let crate_name = normalize_crate_name(target_name);
    let (filename, compiler_artifact_name) = match kind {
        TargetKind::Binary => {
            let compiler_name = rustc_output_filename(compiler, target, &crate_name, "bin")?;
            let suffix = compiler_name.strip_prefix(&crate_name).ok_or_else(|| {
                WarpError::new(format!(
                    "rustc artifact name {compiler_name:?} does not begin with crate name {crate_name:?}"
                ))
            })?;
            (format!("{target_name}{suffix}"), compiler_name)
        }
        TargetKind::Library => {
            let name = format!("lib{crate_name}.rlib");
            (name.clone(), name)
        }
        TargetKind::ProcMacro => {
            let name = format!(
                "{}{}{}",
                dynamic_library_prefix(&crate_name),
                crate_name,
                dynamic_library_suffix(&crate_name)
            );
            (name.clone(), name)
        }
    };
    Ok((
        build_directory.join(package_relative).join(filename),
        compiler_artifact_name,
    ))
}

fn task_id(package_id: &str, target_name: &str) -> Result<TaskId> {
    TaskId::new(format!("{package_id}::{target_name}"))
        .map_err(|error| WarpError::new(error.to_string()))
}

fn normalize_crate_name(name: &str) -> String {
    name.replace('-', "_")
}

fn short_hash(value: &str) -> String {
    blake3::hash(value.as_bytes()).to_hex()[..12].to_string()
}

fn dynamic_library_prefix(name: &str) -> String {
    if cfg!(windows) {
        String::new()
    } else {
        format!("lib{}", normalize_crate_name(name))
    }
}

fn dynamic_library_suffix(_name: &str) -> String {
    if cfg!(target_os = "macos") {
        ".dylib".into()
    } else if cfg!(windows) {
        ".dll".into()
    } else {
        ".so".into()
    }
}

fn promote_binary(source: &Path, output: &Path) -> Result<()> {
    if source == output {
        return Ok(());
    }
    let parent = output
        .parent()
        .filter(|parent| !parent.as_os_str().is_empty())
        .unwrap_or_else(|| Path::new("."));
    fs::create_dir_all(parent)
        .map_err(|error| WarpError::io("create promoted binary directory", error))?;
    let temporary = output.with_file_name(format!(
        ".{}.{}.tmp",
        output
            .file_name()
            .and_then(|name| name.to_str())
            .unwrap_or("warp"),
        std::process::id()
    ));
    fs::copy(source, &temporary).map_err(|error| WarpError::io("copy promoted binary", error))?;
    mark_executable(&temporary)?;
    #[cfg(windows)]
    if output.exists() {
        fs::remove_file(output).map_err(|error| WarpError::io("replace promoted binary", error))?;
    }
    fs::rename(&temporary, output).map_err(|error| WarpError::io("publish promoted binary", error))
}

#[cfg(unix)]
fn mark_executable(path: &Path) -> Result<()> {
    use std::os::unix::fs::PermissionsExt;
    let mut permissions = fs::metadata(path)
        .map_err(|error| WarpError::io("inspect workspace binary permissions", error))?
        .permissions();
    permissions.set_mode(0o755);
    fs::set_permissions(path, permissions)
        .map_err(|error| WarpError::io("make workspace binary executable", error))
}

#[cfg(not(unix))]
fn mark_executable(_path: &Path) -> Result<()> {
    Ok(())
}

struct WorkDirectory {
    path: PathBuf,
}

impl WorkDirectory {
    fn create(parent: &Path) -> Result<Self> {
        fs::create_dir_all(parent)
            .map_err(|error| WarpError::io("create workspace work root", error))?;
        loop {
            let id = NEXT_WORK.fetch_add(1, Ordering::Relaxed);
            let path = parent.join(format!(".warp-work-{}-{id}", std::process::id()));
            match fs::create_dir(&path) {
                Ok(()) => return Ok(Self { path }),
                Err(error) if error.kind() == std::io::ErrorKind::AlreadyExists => continue,
                Err(error) => return Err(WarpError::io("create workspace work directory", error)),
            }
        }
    }
}

impl Drop for WorkDirectory {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.path);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn package_artifacts_mirror_workspace_paths() {
        let build_directory = Path::new("/workspace/_build/aarch64-apple-darwin/release");
        let package = Path::new("crates/warp-store");
        let (compiler, _) = rustc().unwrap();
        let target = crate::host_target().unwrap();
        let (library, _) = artifact_output(
            TargetKind::Library,
            "warp-store",
            package,
            build_directory,
            &target,
            &compiler,
        )
        .unwrap();
        assert_eq!(
            library,
            build_directory
                .join("crates/warp-store")
                .join("libwarp_store.rlib")
        );
        let (binary, compiler_name) = artifact_output(
            TargetKind::Binary,
            "tiny-tool",
            package,
            build_directory,
            &target,
            &compiler,
        )
        .unwrap();
        let suffix = compiler_name.strip_prefix("tiny_tool").unwrap();
        assert_eq!(
            binary,
            build_directory
                .join("crates/warp-store")
                .join(format!("tiny-tool{suffix}"))
        );
    }
}
