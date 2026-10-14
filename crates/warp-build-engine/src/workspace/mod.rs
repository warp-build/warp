use crate::action_cache::ActionCache;
use crate::error::{Result, WarpError};
use crate::graph::{ActionPlan, GraphExecutor, Plan, TaskId, TaskProvider};
use crate::request::{rustc_flags, target_environment_name};
use crate::{default_cache_directory, status_tag, BuildProfile, CompilationTargetTriple};
use ignore::WalkBuilder;
use serde::Deserialize;
use std::collections::{HashMap, HashSet};
use std::fs;
use std::num::NonZeroUsize;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Mutex, OnceLock};
use std::time::{Duration, Instant};
use warp_store::{BlobDigest, Cas};

mod build_client_config;
mod build_ctx;
mod build_intent;
mod build_node;
mod build_runtime;
mod build_target;
mod hashing;
mod label;
mod scan;
#[cfg(unix)]
mod unix;
#[cfg(windows)]
mod windows;
#[cfg(not(any(unix, windows)))]
compile_error!("Warp workspace builds support Unix and Windows hosts only");

const GENERATED_OUTPUT_DIRECTORY: &str = "_build";

#[cfg(unix)]
use self::unix as platform;
#[cfg(windows)]
use self::windows as platform;
pub use build_client_config::BuildClientConfig;
pub use build_ctx::BuildCtx;
pub use build_intent::{BuildIntent, BuildScope};
use build_node::BuildNode;
use build_runtime::BuildRuntime;
pub use build_target::BuildTarget;
use hashing::HashPool;
pub use label::Label;

static NEXT_WORK: AtomicU64 = AtomicU64::new(0);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct WorkspaceReport {
    pub output: Option<PathBuf>,
    pub built: usize,
    pub cached: usize,
    pub targets: usize,
    pub elapsed: Duration,
}

const BUILD_INTENT_TASK: &str = "warp:build-intent";
const BUILD_SCOPE_TASK: &str = "warp:resolve-scope";

pub fn build_workspace(intent: &BuildIntent) -> Result<WorkspaceReport> {
    let context = Arc::new(BuildCtx::new(intent.clone()));
    let provider = WorkspaceProvider::new(context.clone());
    let root = TaskId::new(BUILD_INTENT_TASK).map_err(|error| WarpError::new(error.to_string()))?;
    let workers = std::thread::available_parallelism().unwrap_or(NonZeroUsize::MIN);
    let graph = GraphExecutor::new(workers);

    let result = graph.execute(&provider, [root]);
    match result {
        Ok(_execution) => Ok(WorkspaceReport {
            output: context.output(),
            built: provider.built.load(Ordering::Relaxed) as usize,
            cached: provider.cached.load(Ordering::Relaxed) as usize,
            targets: (provider.built.load(Ordering::Relaxed)
                + provider.cached.load(Ordering::Relaxed)) as usize,
            elapsed: context.started.elapsed(),
        }),
        Err(error) => {
            eprintln!(
                "{} build failed in {:.2}s",
                status_tag("FINISHED", false),
                context.started.elapsed().as_secs_f64()
            );
            Err(WarpError::new(format!("workspace graph failed: {error}")))
        }
    }
}

fn initialize_runtime(intent: &BuildIntent) -> Result<BuildRuntime> {
    let root = intent
        .root
        .canonicalize()
        .map_err(|error| WarpError::io("resolve workspace root", error))?;
    let scope = match &intent.scope {
        BuildScope::All => BuildScope::All,
        BuildScope::Labels(labels) if labels.is_empty() => {
            return Err(WarpError::new(
                "a scoped build needs at least one target label",
            ));
        }
        BuildScope::Labels(labels) => BuildScope::Labels(labels.clone()),
    };
    let metadata = load_metadata(
        &root,
        &intent.compilation_target,
        intent
            .rust_toolchain
            .as_ref()
            .map(|channel| channel.as_str()),
    )?;
    let root = metadata.workspace_root.clone();
    let build_directory = root
        .join(GENERATED_OUTPUT_DIRECTORY)
        .join(intent.compilation_target.as_str())
        .join(intent.profile.as_str());
    fs::create_dir_all(&build_directory)
        .map_err(|error| WarpError::io("create target/profile build directory", error))?;

    let requested_cache = match &intent.cache {
        Some(path) => path.clone(),
        None => default_cache_directory()?,
    };
    let cache_root = absolute_directory(&requested_cache, "create workspace cache")?;
    if intent.cache.is_some() && cache_root.starts_with(&root) {
        return Err(WarpError::new(
            "workspace cache override must be outside the workspace source tree",
        ));
    }
    if let Some(output) = &intent.output {
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
    let action_cache = ActionCache::new(cache_root.join("actions"))?;
    let cas = Cas::new(cache_root.join("cas"))
        .map_err(|error| WarpError::new(format!("open content store: {error}")))?;

    let work = WorkDirectory::create(&cache_root)?;
    let workspace = Workspace::from_metadata(
        metadata,
        WorkspaceBuildContext {
            root: &root,
            work_root: &work.path,
            build_directory: &build_directory,
            compilation_target: intent.compilation_target.clone(),
            profile: intent.profile,
            scope: &scope,
            excluded_directories: &intent.client.excluded_directories,
        },
    )?;

    Ok(BuildRuntime::new(
        workspace,
        work,
        action_cache,
        cas,
        intent.rust_toolchain.clone(),
    ))
}

fn load_metadata(
    root: &Path,
    compilation_target: &CompilationTargetTriple,
    rust_toolchain: Option<&str>,
) -> Result<CargoMetadata> {
    let mut command = Command::new("cargo");
    command.current_dir(root);
    if let Some(toolchain) = rust_toolchain {
        command.env("RUSTUP_TOOLCHAIN", toolchain).env(
            "CARGO_TARGET_DIR",
            root.join(GENERATED_OUTPUT_DIRECTORY)
                .join("cargo")
                .join(toolchain),
        );
    }
    let output = command
        .args([
            "metadata",
            "--locked",
            "--format-version",
            "1",
            "--filter-platform",
        ])
        .arg(compilation_target.as_str())
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
    outputs: Vec<String>,
    toolchains: Vec<String>,
}

const RUST_TARGET_RULE: &str = warp_rules::RUST_TARGET_RULE;

#[derive(Clone)]
struct BuildDependency {
    alias: String,
    package_id: String,
    internal_task: Option<TaskId>,
    external_artifact: Option<ExternalArtifact>,
}

#[derive(Clone)]
struct ExternalArtifact {
    package_name: String,
    version: String,
    crate_name: String,
    proc_macro: bool,
    features: Vec<String>,
}

struct Workspace {
    root: PathBuf,
    build_directory: PathBuf,
    work_directory: PathBuf,
    dependency_directories: Vec<PathBuf>,
    cargo_target_directory: PathBuf,
    target_profile_directories: Vec<PathBuf>,
    host_profile_directories: Vec<PathBuf>,
    target_native_directories: OnceLock<std::result::Result<Vec<PathBuf>, String>>,
    host_native_directories: OnceLock<std::result::Result<Vec<PathBuf>, String>>,
    global_fingerprint: OnceLock<std::result::Result<String, String>>,
    compilation_target: CompilationTargetTriple,
    host_triple: CompilationTargetTriple,
    profile: BuildProfile,
    targets: HashMap<TaskId, BuildTarget>,
    roots: Vec<TaskId>,
    excluded_directories: Vec<String>,
    external_artifacts: Mutex<HashMap<String, PathBuf>>,
    hash_pool: HashPool,
}

struct WorkspaceBuildContext<'a> {
    root: &'a Path,
    work_root: &'a Path,
    build_directory: &'a Path,
    compilation_target: CompilationTargetTriple,
    profile: BuildProfile,
    scope: &'a BuildScope,
    excluded_directories: &'a [String],
}

impl Workspace {
    fn from_metadata(metadata: CargoMetadata, context: WorkspaceBuildContext<'_>) -> Result<Self> {
        let WorkspaceBuildContext {
            root,
            work_root,
            build_directory,
            compilation_target,
            profile,
            scope,
            excluded_directories,
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
        let selected_targets =
            select_workspace_targets(scope, &packages, &workspace_member_ids, root)?;
        let selected_package_ids = selected_targets.keys().cloned().collect::<HashSet<_>>();
        let active_package_ids =
            reachable_workspace_packages(&selected_package_ids, &workspace_member_ids, &nodes);
        let host = guess_host_triple::guess_host_triple()
            .ok_or_else(|| WarpError::new("could not detect the host target triple"))?;
        let host_triple = CompilationTargetTriple::parse(host)?;
        let host_profile_directory = metadata.target_directory.join(profile.as_str());
        let target_profile_directory = metadata
            .target_directory
            .join(compilation_target.as_str())
            .join(profile.as_str());
        let target_profile_directories = if compilation_target == host_triple {
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
                if selected_targets.get(package_id).is_some_and(|selection| {
                    selection
                        .as_ref()
                        .is_none_or(|target_names| target_names.contains(&target.name))
                }) {
                    roots.push(id);
                }
            }
        }
        roots.sort();

        if target_ids.is_empty() {
            return Err(WarpError::new(
                "selected packages have no supported Rust targets",
            ));
        }
        let mut targets = HashMap::new();
        let mut source_fingerprints = HashMap::new();
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
                            external_artifact: None,
                        })
                    } else {
                        let external_package = packages.get(&dependency.pkg).ok_or_else(|| {
                            WarpError::new(format!(
                                "Cargo metadata omitted package {}",
                                dependency.pkg
                            ))
                        })?;
                        let crate_target = external_package
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
                                    external_package.name
                                ))
                            })?;
                        let external_features = nodes
                            .get(&dependency.pkg)
                            .map(|node| node.features.clone())
                            .unwrap_or_default();
                        Ok(BuildDependency {
                            alias: normalize_crate_name(&dependency.name),
                            package_id: dependency.pkg,
                            internal_task: None,
                            external_artifact: Some(ExternalArtifact {
                                package_name: external_package.name.clone(),
                                version: external_package.version.clone(),
                                crate_name: crate_target.name.clone(),
                                proc_macro: crate_target
                                    .kind
                                    .iter()
                                    .any(|kind| kind == "proc-macro"),
                                features: external_features,
                            }),
                        })
                    }
                })
                .collect::<Result<Vec<_>>>()?;
            let features = node.features.clone();
            let source_fingerprint = source_fingerprints
                .entry(package_id.clone())
                .or_insert_with(|| Arc::new(OnceLock::new()))
                .clone();
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
                    &host_triple
                } else {
                    &compilation_target
                };
                let (output, compiler_artifact_name) = artifact_output(
                    *kind,
                    &target.name,
                    &package_relative,
                    build_directory,
                    effective_target,
                )?;
                targets.insert(
                    id.clone(),
                    BuildTarget {
                        label: Label::parse(format!("{}::{}", package.name, target.name))?,
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
        Ok(Self {
            root: root.to_path_buf(),
            build_directory: build_directory.to_path_buf(),
            work_directory: work_root.to_path_buf(),
            dependency_directories,
            cargo_target_directory: metadata.target_directory,
            target_profile_directories,
            host_profile_directories,
            target_native_directories: OnceLock::new(),
            host_native_directories: OnceLock::new(),
            global_fingerprint: OnceLock::new(),
            compilation_target,
            host_triple,
            profile,
            targets,
            roots,
            excluded_directories: excluded_directories.to_vec(),
            external_artifacts: Mutex::new(HashMap::new()),
            hash_pool: HashPool::new()?,
        })
    }

    fn native_directories(&self, for_host: bool) -> Result<&[PathBuf]> {
        let (cache, profile_directories) = if for_host {
            (
                &self.host_native_directories,
                &self.host_profile_directories,
            )
        } else {
            (
                &self.target_native_directories,
                &self.target_profile_directories,
            )
        };
        cache
            .get_or_init(|| {
                let mut directories = Vec::new();
                for profile_directory in profile_directories {
                    directories.extend(
                        collect_native_directories(&profile_directory.join("build"))
                            .map_err(|error| error.to_string())?,
                    );
                }
                directories.sort();
                directories.dedup();
                Ok(directories)
            })
            .as_ref()
            .map(Vec::as_slice)
            .map_err(|error| WarpError::new(error.clone()))
    }

    fn global_fingerprint(&self) -> Result<&str> {
        self.global_fingerprint
            .get_or_init(|| {
                self.compute_global_fingerprint()
                    .map_err(|error| error.to_string())
            })
            .as_ref()
            .map(String::as_str)
            .map_err(|error| WarpError::new(error.clone()))
    }

    fn artifact_digest(&self, path: &Path) -> Result<String> {
        self.hash_pool.hash_file(path)
    }

    fn artifact_digests(&self, paths: &[PathBuf]) -> Result<Vec<String>> {
        self.hash_pool.hash_files(paths)
    }

    fn remember_artifact_digest(&self, path: &Path, digest: &str) -> Result<()> {
        self.hash_pool.remember(path, digest)
    }

    fn external_artifact(&self, package_id: &str, artifact: &ExternalArtifact) -> Result<PathBuf> {
        if let Some(path) = self
            .external_artifacts
            .lock()
            .map_err(|_| WarpError::new("external artifact cache poisoned"))?
            .get(package_id)
            .cloned()
        {
            return Ok(path);
        }
        let profile_directories = if artifact.proc_macro {
            &self.host_profile_directories
        } else {
            &self.target_profile_directories
        };
        let path = find_external_artifact(ExternalArtifactRequest {
            profile_directories,
            cargo_target_directory: &self.cargo_target_directory,
            package_name: &artifact.package_name,
            version: &artifact.version,
            crate_name: &artifact.crate_name,
            proc_macro: artifact.proc_macro,
            features: &artifact.features,
            compilation_target: &self.compilation_target,
            profile: self.profile,
        })?;
        self.external_artifacts
            .lock()
            .map_err(|_| WarpError::new("external artifact cache poisoned"))?
            .insert(package_id.to_string(), path.clone());
        Ok(path)
    }

    fn compute_global_fingerprint(&self) -> Result<String> {
        let mut native_directories = self
            .native_directories(false)?
            .iter()
            .chain(self.native_directories(true)?)
            .cloned()
            .collect::<Vec<_>>();
        native_directories.sort();
        native_directories.dedup();

        let mut hasher = blake3::Hasher::new();
        write_field(&mut hasher, b"warp-global-fingerprint-v2");
        write_field(&mut hasher, RUST_TARGET_RULE.as_bytes());
        write_field(&mut hasher, self.compilation_target.as_str().as_bytes());
        write_field(&mut hasher, self.profile.as_str().as_bytes());
        let mut input_files = [
            self.root.join("Cargo.toml"),
            self.root.join("Cargo.lock"),
            self.root.join("rust-toolchain.toml"),
            self.root.join(".cargo/config.toml"),
            self.root.join(".cargo/config"),
        ]
        .into_iter()
        .filter(|path| path.is_file())
        .collect::<Vec<_>>();
        let mut ignore_files = HashSet::new();
        for target in self.targets.values() {
            let mut directory = target.package_root.as_path();
            loop {
                let ignore_file = directory.join(".gitignore");
                if ignore_file.is_file() {
                    ignore_files.insert(ignore_file);
                }
                if directory == self.root {
                    break;
                }
                let Some(parent) = directory
                    .parent()
                    .filter(|parent| parent.starts_with(&self.root))
                else {
                    break;
                };
                directory = parent;
            }
        }
        input_files.extend(ignore_files);
        input_files.sort();
        input_files.dedup();
        hash_paths_into(&mut hasher, &self.root, &input_files, &self.hash_pool)?;
        for directory in &native_directories {
            let relative = directory
                .strip_prefix(&self.cargo_target_directory)
                .map_err(|_| {
                    WarpError::new("Cargo native output directory escaped the target directory")
                })?;
            write_field(
                &mut hasher,
                relative.to_string_lossy().replace('\\', "/").as_bytes(),
            );
            hash_tree_into(
                &mut hasher,
                directory,
                &self.excluded_directories,
                &self.hash_pool,
            )?;
        }
        for name in ["RUSTFLAGS", "CARGO_ENCODED_RUSTFLAGS", "CARGO_TARGET_DIR"] {
            if let Some(value) = std::env::var_os(name) {
                write_field(&mut hasher, name.as_bytes());
                write_field(&mut hasher, value.to_string_lossy().as_bytes());
            }
        }
        for effective_target in [&self.compilation_target, &self.host_triple] {
            for suffix in ["LINKER", "RUSTFLAGS"] {
                let name = target_environment_name(effective_target, suffix);
                if let Some(value) = std::env::var_os(&name) {
                    write_field(&mut hasher, name.as_bytes());
                    write_field(&mut hasher, value.to_string_lossy().as_bytes());
                }
            }
        }
        Ok(hasher.finalize().to_hex().to_string())
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

fn select_workspace_targets(
    scope: &BuildScope,
    packages: &HashMap<String, CargoPackage>,
    workspace_members: &HashSet<String>,
    workspace_root: &Path,
) -> Result<HashMap<String, Option<HashSet<String>>>> {
    let mut selected = HashMap::new();
    match scope {
        BuildScope::All => {
            for package_id in workspace_members {
                selected.insert(package_id.clone(), None);
            }
        }
        BuildScope::Labels(labels) => {
            if labels.is_empty() {
                return Err(WarpError::new(
                    "a scoped build needs at least one target label",
                ));
            }
            for label in labels {
                select_label(
                    label,
                    packages,
                    workspace_members,
                    workspace_root,
                    &mut selected,
                )?;
            }
        }
    }
    Ok(selected)
}

fn select_label(
    label: &Label,
    packages: &HashMap<String, CargoPackage>,
    workspace_members: &HashSet<String>,
    workspace_root: &Path,
    selected: &mut HashMap<String, Option<HashSet<String>>>,
) -> Result<()> {
    let value = label.as_str();
    if let Some((package_name, target_name)) = value.split_once("::") {
        let (package_id, package) = workspace_members
            .iter()
            .filter_map(|package_id| {
                packages
                    .get(package_id)
                    .map(|package| (package_id, package))
            })
            .find(|(_, package)| package.name == package_name)
            .ok_or_else(|| WarpError::new(format!("no build target matches label {label}")))?;
        if !package
            .targets
            .iter()
            .any(|target| target.name == target_name)
        {
            return Err(WarpError::new(format!(
                "package {package_name:?} has no Cargo target named {target_name:?}"
            )));
        }
        include_selected_target(selected, package_id, Some(target_name));
        return Ok(());
    }

    if let Some(package_id) = workspace_members.iter().find(|package_id| {
        packages
            .get(*package_id)
            .is_some_and(|package| package.name == value)
    }) {
        include_selected_target(selected, package_id, None);
        return Ok(());
    }

    let path = Path::new(value);
    let looks_like_path =
        path.is_absolute() || value.starts_with('.') || value.contains('/') || value.contains('\\');
    if !looks_like_path {
        return Err(WarpError::new(format!(
            "no build target matches label {label}"
        )));
    }
    let requested_path = if path.is_absolute() {
        path.to_path_buf()
    } else {
        workspace_root.join(path)
    };
    let requested_path = requested_path
        .canonicalize()
        .map_err(|error| WarpError::io("resolve build-target label", error))?;
    let package_ids = select_workspace_packages(
        packages,
        workspace_members,
        workspace_root,
        Some(&requested_path),
    )?;
    for package_id in package_ids {
        include_selected_target(selected, &package_id, None);
    }
    Ok(())
}

fn include_selected_target(
    selected: &mut HashMap<String, Option<HashSet<String>>>,
    package_id: &str,
    target_name: Option<&str>,
) {
    match target_name {
        None => {
            selected.insert(package_id.to_string(), None);
        }
        Some(target_name) => {
            let selection = selected
                .entry(package_id.to_string())
                .or_insert_with(|| Some(HashSet::new()));
            if let Some(target_names) = selection {
                target_names.insert(target_name.to_string());
            }
        }
    }
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
    cargo_target_directory: &'a Path,
    package_name: &'a str,
    version: &'a str,
    crate_name: &'a str,
    proc_macro: bool,
    features: &'a [String],
    compilation_target: &'a CompilationTargetTriple,
    profile: BuildProfile,
}

fn find_external_artifact(request: ExternalArtifactRequest<'_>) -> Result<PathBuf> {
    let ExternalArtifactRequest {
        profile_directories,
        cargo_target_directory,
        package_name,
        version,
        crate_name,
        proc_macro,
        features,
        compilation_target,
        profile,
    } = request;
    let extension = if proc_macro {
        platform::dynamic_library_extension()
    } else {
        "rlib"
    };
    let crate_name = normalize_crate_name(crate_name);
    let prefix = if proc_macro {
        format!("{}-", platform::dynamic_library_prefix(&crate_name))
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
            "no prebuilt Cargo artifact for {package_name} {version} ({crate_name}) and compilation target {}; build dependencies with Cargo using the same Rust toolchain and target directory {} (`{cargo_profile} --target {}`)",
            compilation_target.as_str(),
            cargo_target_directory.display(),
            compilation_target.as_str()
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
    context: Arc<BuildCtx>,
    announced: std::sync::atomic::AtomicBool,
    nodes: Mutex<HashMap<TaskId, BuildNode>>,
    scan_dependencies: Mutex<HashMap<TaskId, Vec<TaskId>>>,
    built: AtomicU64,
    cached: AtomicU64,
}

impl WorkspaceProvider {
    fn new(context: Arc<BuildCtx>) -> Self {
        Self {
            context,
            announced: std::sync::atomic::AtomicBool::new(false),
            nodes: Mutex::new(HashMap::new()),
            scan_dependencies: Mutex::new(HashMap::new()),
            built: AtomicU64::new(0),
            cached: AtomicU64::new(0),
        }
    }

    fn runtime(&self) -> std::result::Result<&BuildRuntime, String> {
        self.context
            .runtime(|| initialize_runtime(&self.context.intent))
    }

    fn register_node(&self, node: BuildNode) -> std::result::Result<TaskId, String> {
        let (prefix, path) = match &node {
            BuildNode::ScanDirectory(path) => ("warp:scan", path),
            BuildNode::PackagePath(path) => ("warp:package", path),
        };
        let identity = path.to_string_lossy();
        let id = TaskId::new(format!("{prefix}:{}", short_hash(&identity)))
            .map_err(|error| error.to_string())?;
        self.nodes
            .lock()
            .map_err(|_| "workspace graph node registry poisoned".to_string())?
            .entry(id.clone())
            .or_insert(node);
        Ok(id)
    }

    fn intent_dependency(&self) -> std::result::Result<TaskId, String> {
        match &self.context.intent.scope {
            BuildScope::All => {
                if self.context.intent.verbose {
                    eprintln!("[graph] BuildIntent expands to a workspace scan");
                }
                self.register_node(BuildNode::ScanDirectory(self.context.intent.root.clone()))
            }
            BuildScope::Labels(labels) => {
                if self.context.intent.verbose {
                    eprintln!(
                        "[graph] BuildIntent resolves {} target label(s)",
                        labels.len()
                    );
                }
                TaskId::new(BUILD_SCOPE_TASK).map_err(|error| error.to_string())
            }
        }
    }

    fn scan_directory_dependencies(
        &self,
        task: &TaskId,
        directory: &Path,
    ) -> std::result::Result<Vec<TaskId>, String> {
        if let Some(dependencies) = self
            .scan_dependencies
            .lock()
            .map_err(|_| "workspace scan state poisoned".to_string())?
            .get(task)
            .cloned()
        {
            return Ok(dependencies);
        }

        let scan_started = Instant::now();
        let directory = directory
            .canonicalize()
            .map_err(|error| format!("resolve scan path {}: {error}", directory.display()))?;
        let (has_manifest, child_directories) =
            scan::discover_directory(&directory, &self.context.intent.client.excluded_directories)
                .map_err(|error| error.to_string())?;
        if self.context.intent.verbose {
            eprintln!(
                "[graph] scanned {} (manifest: {}, child directories: {}, {}ms)",
                directory.display(),
                has_manifest,
                child_directories.len(),
                scan_started.elapsed().as_millis()
            );
        }
        let mut dependencies = Vec::new();
        if has_manifest {
            dependencies.push(self.register_node(BuildNode::PackagePath(directory.clone()))?);
        }
        for child in child_directories {
            dependencies.push(self.register_node(BuildNode::ScanDirectory(child))?);
        }
        dependencies.sort();
        dependencies.dedup();
        self.scan_dependencies
            .lock()
            .map_err(|_| "workspace scan state poisoned".to_string())?
            .insert(task.clone(), dependencies.clone());
        Ok(dependencies)
    }

    fn plan_children(
        &self,
        task: &TaskId,
        children: &[TaskId],
        completed_dependencies: &[TaskId],
    ) -> std::result::Result<Plan, String> {
        let completed: HashSet<&TaskId> = completed_dependencies.iter().collect();
        let missing = children
            .iter()
            .filter(|child| !completed.contains(child))
            .cloned()
            .collect::<Vec<_>>();
        if missing.is_empty() {
            Ok(Plan::Ready(ActionPlan::new(
                task.as_str(),
                completed_dependencies.to_vec(),
            )))
        } else {
            Ok(Plan::NeedsDependencies(missing))
        }
    }

    fn ensure_source_fingerprint(
        &self,
        target: &BuildTarget,
        hash_pool: &HashPool,
    ) -> std::result::Result<(), String> {
        let started = Instant::now();
        let result = target
            .source_fingerprint
            .get_or_init(|| {
                copy_and_fingerprint_tree(
                    &target.package_root,
                    &target.source_root,
                    &self.context.intent.client.excluded_directories,
                    hash_pool,
                )
                .map_err(|error| error.to_string())
            })
            .as_ref()
            .map(|_| ())
            .map_err(Clone::clone);
        if self.context.intent.verbose {
            eprintln!(
                "[profile] source snapshot {}={}ms",
                target.label(),
                started.elapsed().as_millis()
            );
        }
        result
    }
}

impl TaskProvider for WorkspaceProvider {
    fn plan(
        &self,
        task: &TaskId,
        completed_dependencies: &[TaskId],
    ) -> std::result::Result<Plan, String> {
        let completed: HashSet<&TaskId> = completed_dependencies.iter().collect();
        if task.as_str() == BUILD_INTENT_TASK {
            let dependency = self.intent_dependency()?;
            return if completed.contains(&dependency) {
                Ok(Plan::Ready(ActionPlan::new(
                    task.as_str(),
                    completed_dependencies.to_vec(),
                )))
            } else {
                Ok(Plan::NeedsDependencies(vec![dependency]))
            };
        }

        if task.as_str() == BUILD_SCOPE_TASK {
            let runtime = self.runtime()?;
            if runtime.workspace.roots.is_empty() {
                return Err("scoped build selected no supported Rust targets".into());
            }
            if !self.announced.swap(true, Ordering::Relaxed) {
                let building = runtime
                    .workspace
                    .roots
                    .iter()
                    .filter_map(|root| runtime.workspace.targets.get(root))
                    .map(|target| target.label().to_string())
                    .collect::<Vec<_>>();
                eprintln!("Building {}", building.join(" "));
            }
            return self.plan_children(task, &runtime.workspace.roots, completed_dependencies);
        }

        let node = self
            .nodes
            .lock()
            .map_err(|_| "workspace graph node registry poisoned".to_string())?
            .get(task)
            .cloned();
        match node {
            Some(BuildNode::ScanDirectory(directory)) => {
                let children = self.scan_directory_dependencies(task, &directory)?;
                self.plan_children(task, &children, completed_dependencies)
            }
            Some(BuildNode::PackagePath(path)) => {
                let runtime = self.runtime()?;
                if !self.announced.swap(true, Ordering::Relaxed) {
                    let building = runtime
                        .workspace
                        .roots
                        .iter()
                        .filter_map(|root| runtime.workspace.targets.get(root))
                        .map(|target| target.label().to_string())
                        .collect::<Vec<_>>();
                    eprintln!("Building {}", building.join(" "));
                }
                let path = path
                    .canonicalize()
                    .map_err(|error| format!("resolve Cargo package path: {error}"))?;
                let workspace = &runtime.workspace;
                let roots = if path == workspace.root {
                    workspace.roots.clone()
                } else {
                    workspace
                        .roots
                        .iter()
                        .filter(|root| {
                            workspace
                                .targets
                                .get(*root)
                                .is_some_and(|target| target.package_root == path)
                        })
                        .cloned()
                        .collect()
                };
                self.plan_children(task, &roots, completed_dependencies)
            }
            None => {
                let runtime = self.runtime()?;
                let target = runtime
                    .workspace
                    .targets
                    .get(task)
                    .ok_or_else(|| format!("unknown workspace task {task}"))?;
                self.ensure_source_fingerprint(target, &runtime.workspace.hash_pool)?;
                if self.context.intent.verbose {
                    eprintln!(
                        "[graph] planned {} with {} discovered dependencies",
                        target.label(),
                        target.dependencies.len()
                    );
                }
                let dependencies = target
                    .dependencies
                    .iter()
                    .filter_map(|dependency| dependency.internal_task.as_ref())
                    .cloned()
                    .collect::<Vec<_>>();
                self.plan_children(task, &dependencies, completed_dependencies)
            }
        }
    }

    fn execute(&self, task: &TaskId, _action: &ActionPlan) -> std::result::Result<(), String> {
        if task.as_str() == BUILD_INTENT_TASK {
            return self.finish_intent();
        }
        if task.as_str() == BUILD_SCOPE_TASK
            || self
                .nodes
                .lock()
                .map_err(|_| "workspace graph node registry poisoned".to_string())?
                .contains_key(task)
        {
            return Ok(());
        }
        let started = Instant::now();
        let label = self
            .runtime()?
            .workspace
            .targets
            .get(task)
            .map(|target| target.label().to_string())
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
    fn finish_intent(&self) -> std::result::Result<(), String> {
        let runtime = self.runtime()?;
        let workspace = &runtime.workspace;
        let selected_binary = selected_binary(
            workspace,
            self.context.intent.client.default_binary.as_ref(),
        )?;
        let promoted_output = match selected_binary {
            Some(selected_binary) => {
                let output = self
                    .context
                    .intent
                    .output
                    .as_ref()
                    .map(|path| {
                        if path.is_absolute() {
                            path.clone()
                        } else {
                            workspace.root.join(path)
                        }
                    })
                    .unwrap_or_else(|| {
                        workspace.build_directory.join("bin").join(
                            selected_binary
                                .output
                                .file_name()
                                .expect("binary artifact has a filename"),
                        )
                    });
                promote_binary(&selected_binary.output, &output)
                    .map_err(|error| error.to_string())?;
                Some(output)
            }
            None if self.context.intent.output.is_some() => {
                return Err("an output path was requested without a selected binary target".into());
            }
            None if matches!(&self.context.intent.scope, BuildScope::All)
                && self.context.intent.client.default_binary.is_some() =>
            {
                return Err(format!(
                    "client-configured binary {:?} was not found among the selected targets",
                    self.context.intent.client.default_binary
                ));
            }
            None => None,
        };
        if self.context.intent.verbose {
            let hashes = workspace.hash_pool.stats();
            eprintln!(
                "[profile] content hashing files={} bytes={} workers={} summed={}ms",
                hashes.files_hashed,
                hashes.bytes_hashed,
                hashes.thread_count,
                hashes.elapsed.as_millis()
            );
        }
        self.context.set_output(promoted_output);
        Ok(())
    }

    fn execute_target(&self, task: &TaskId) -> std::result::Result<TargetOutcome, String> {
        let runtime = self.runtime()?;
        let workspace = &runtime.workspace;
        let target = workspace
            .targets
            .get(task)
            .ok_or_else(|| format!("unknown workspace task {task}"))?;
        let dependencies_started = Instant::now();
        let dependencies = self
            .resolve_dependencies(runtime, target)
            .map_err(|error| error.to_string())?;
        let dependencies_elapsed = dependencies_started.elapsed();
        let (compiler, compiler_version) = runtime.compiler()?;
        let key_started = Instant::now();
        let key = target_action_key(workspace, target, &dependencies, compiler, compiler_version)
            .map_err(|error| error.to_string())?;
        let key_elapsed = key_started.elapsed();
        let lookup_started = Instant::now();
        let cached_blob = runtime
            .action_cache
            .lookup(&key)
            .map_err(|error| error.to_string())?;
        let lookup_elapsed = lookup_started.elapsed();

        if let Some(blob) = cached_blob {
            let restore_started = Instant::now();
            if self
                .restore(runtime, &blob, target)
                .map_err(|error| error.to_string())?
            {
                if self.context.intent.verbose {
                    eprintln!(
                        "[profile] {} cache-hit deps={}ms key={}ms lookup={}ms restore={}ms",
                        target.label(),
                        dependencies_elapsed.as_millis(),
                        key_elapsed.as_millis(),
                        lookup_elapsed.as_millis(),
                        restore_started.elapsed().as_millis()
                    );
                }
                self.cached.fetch_add(1, Ordering::Relaxed);
                return Ok(TargetOutcome::Cached);
            }
        }
        if self.context.intent.verbose {
            eprintln!(
                "[profile] {} cache-miss deps={}ms key={}ms lookup={}ms",
                target.label(),
                dependencies_elapsed.as_millis(),
                key_elapsed.as_millis(),
                lookup_elapsed.as_millis()
            );
        }

        let work = WorkDirectory::create(&workspace.work_directory.join("outputs"))
            .map_err(|error| error.to_string())?;
        let artifact = compile_target(workspace, target, &dependencies, compiler, &work.path)
            .map_err(|error| error.to_string())?;
        let blob = runtime
            .cas
            .put_file(&artifact)
            .map_err(|error| error.to_string())?;
        runtime
            .action_cache
            .insert(&key, &blob)
            .map_err(|error| error.to_string())?;
        if !self
            .restore(runtime, &blob, target)
            .map_err(|error| error.to_string())?
        {
            return Err(format!("CAS lost compiled workspace artifact for {task}"));
        }
        self.built.fetch_add(1, Ordering::Relaxed);
        Ok(TargetOutcome::Built)
    }

    fn resolve_dependencies(
        &self,
        runtime: &BuildRuntime,
        target: &BuildTarget,
    ) -> Result<Vec<(String, String, PathBuf)>> {
        target
            .dependencies
            .iter()
            .map(|dependency| {
                let path = if let Some(task) = &dependency.internal_task {
                    runtime
                        .workspace
                        .targets
                        .get(task)
                        .map(|target| target.output.clone())
                        .ok_or_else(|| {
                            WarpError::new(format!("missing workspace dependency target {task}"))
                        })?
                } else {
                    let artifact = dependency.external_artifact.as_ref().ok_or_else(|| {
                        WarpError::new(format!(
                            "missing Cargo artifact metadata for {}",
                            dependency.package_id
                        ))
                    })?;
                    runtime
                        .workspace
                        .external_artifact(&dependency.package_id, artifact)?
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

    fn restore(
        &self,
        runtime: &BuildRuntime,
        blob: &BlobDigest,
        target: &BuildTarget,
    ) -> Result<bool> {
        if target.output.is_file()
            && runtime.workspace.artifact_digest(&target.output)? == blob.as_str()
        {
            if target.kind == TargetKind::Binary {
                platform::mark_executable(&target.output)
                    .map_err(|error| WarpError::io("set workspace artifact permissions", error))?;
            }
            return Ok(true);
        }
        let restored = runtime
            .cas
            .get_file(blob, &target.output)
            .map_err(|error| WarpError::new(format!("materialize workspace artifact: {error}")))?;
        if restored && target.kind == TargetKind::Binary {
            platform::mark_executable(&target.output)
                .map_err(|error| WarpError::io("set workspace artifact permissions", error))?;
        }
        if restored {
            runtime
                .workspace
                .remember_artifact_digest(&target.output, blob.as_str())?;
        }
        Ok(restored)
    }
}

fn selected_binary<'a>(
    workspace: &'a Workspace,
    selector: Option<&Label>,
) -> std::result::Result<Option<&'a BuildTarget>, String> {
    let mut matches = workspace
        .roots
        .iter()
        .filter_map(|task| workspace.targets.get(task))
        .filter(|target| {
            target.kind == TargetKind::Binary
                && selector.is_none_or(|selector| {
                    target.target_name == selector.as_str() || target.label() == selector
                })
        });
    let selected = matches.next();
    if matches.next().is_some() {
        if let Some(selector) = selector {
            return Err(format!(
                "binary selector {selector:?} is ambiguous; use package::binary"
            ));
        }
        return Ok(None);
    }
    Ok(selected)
}

fn compile_target(
    workspace: &Workspace,
    target: &BuildTarget,
    dependencies: &[(String, String, PathBuf)],
    compiler: &Path,
    output_directory: &Path,
) -> Result<PathBuf> {
    fs::create_dir_all(output_directory)
        .map_err(|error| WarpError::io("create rustc output directory", error))?;
    let effective_target = if target.kind == TargetKind::ProcMacro {
        &workspace.host_triple
    } else {
        &workspace.compilation_target
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
    let native_directories = workspace.native_directories(target.kind == TargetKind::ProcMacro)?;
    for directory in native_directories {
        compiler_arguments.push("-L".to_string());
        compiler_arguments.push(format!("native={}", path_argument(directory)?));
    }
    compiler_arguments.extend(rustc_flags(effective_target));

    let rule_configuration = serde_json::json!({
        "compiler": compiler.to_string_lossy(),
        "crate_name": target.crate_name,
        "crate_type": match target.kind {
            TargetKind::Library => "rlib",
            TargetKind::Binary => "bin",
            TargetKind::ProcMacro => "proc-macro",
        },
        "dependencies": dependencies
            .iter()
            .map(|(alias, _, path)| format!("{alias}={}", path.to_string_lossy()))
            .collect::<Vec<_>>(),
        "edition": target.edition,
        "features": target.features,
        "compiler_arguments": compiler_arguments,
        "output_name": target.compiler_artifact_name,
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
    if Path::new(&action.program) != compiler {
        return Err(WarpError::new(format!(
            "Rust build rule selected unexpected compiler {:?}",
            action.program
        )));
    }
    if action.outputs != [target.compiler_artifact_name.as_str()] {
        return Err(WarpError::new(format!(
            "Rust build rule declared unexpected outputs: {:?}",
            action.outputs
        )));
    }
    if action.toolchains != ["rustc"] {
        return Err(WarpError::new(format!(
            "Rust build rule requires unsupported toolchains: {:?}",
            action.toolchains
        )));
    }

    let mut command = Command::new(&action.program);
    if target.kind != TargetKind::ProcMacro {
        add_host_native_loader_paths(&mut command, workspace.native_directories(true)?)?;
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
    let variable = platform::native_loader_variable();
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
    target: &BuildTarget,
    dependencies: &[(String, String, PathBuf)],
    compiler: &Path,
    compiler_version: &str,
) -> Result<String> {
    let mut hasher = blake3::Hasher::new();
    write_field(&mut hasher, b"warp-cargo-package-action-v2");
    write_field(&mut hasher, workspace.global_fingerprint()?.as_bytes());
    let effective_target = if target.kind == TargetKind::ProcMacro {
        &workspace.host_triple
    } else {
        &workspace.compilation_target
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
    let source_fingerprint = target
        .source_fingerprint
        .get()
        .ok_or_else(|| WarpError::new("target source was not fingerprinted by its graph node"))?
        .as_ref()
        .map_err(|error| WarpError::new(error.clone()))?;
    write_field(&mut hasher, source_fingerprint.as_bytes());
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
    let paths = dependencies
        .iter()
        .map(|(_, _, path)| path.clone())
        .collect::<Vec<_>>();
    let digests = workspace.artifact_digests(&paths)?;
    for ((alias, package_id, _), digest) in dependencies.iter().zip(digests) {
        write_field(&mut hasher, alias.as_bytes());
        write_field(&mut hasher, package_id.as_bytes());
        write_field(&mut hasher, digest.as_bytes());
    }
    Ok(format!("blake3:{}", hasher.finalize().to_hex()))
}

fn copy_and_fingerprint_tree(
    source: &Path,
    destination: &Path,
    excluded_directories: &[String],
    hash_pool: &HashPool,
) -> Result<String> {
    fs::create_dir_all(destination)
        .map_err(|error| WarpError::io("create workspace source snapshot", error))?;
    let mut files = Vec::new();
    collect_files(source, &mut files, excluded_directories, true)?;
    files.sort();
    let mut hasher = blake3::Hasher::new();
    write_field(&mut hasher, b"warp-source-fingerprint-v2");
    let mut snapshot_paths = Vec::with_capacity(files.len());
    for relative in files {
        let source_path = source.join(&relative);
        let destination_path = destination.join(&relative);
        if let Some(parent) = destination_path.parent() {
            fs::create_dir_all(parent)
                .map_err(|error| WarpError::io("create package snapshot directory", error))?;
        }
        fs::copy(&source_path, &destination_path)
            .map_err(|error| WarpError::io("copy package source into immutable snapshot", error))?;
        snapshot_paths.push(destination_path);
    }
    hash_paths_into(&mut hasher, destination, &snapshot_paths, hash_pool)?;
    Ok(hasher.finalize().to_hex().to_string())
}

fn collect_files(
    root: &Path,
    files: &mut Vec<PathBuf>,
    excluded_directories: &[String],
    respect_gitignore: bool,
) -> Result<()> {
    let mut walker = WalkBuilder::new(root);
    let excluded_directories = excluded_directories.to_vec();
    walker
        .standard_filters(false)
        .hidden(false)
        .parents(respect_gitignore)
        .ignore(false)
        .git_ignore(respect_gitignore)
        .git_global(false)
        .git_exclude(false)
        .follow_links(false)
        .filter_entry(move |entry| {
            let is_directory = entry.file_type().is_some_and(|kind| kind.is_dir());
            entry.depth() == 0
                || !is_directory
                || !scan::should_skip_directory(entry.path(), &excluded_directories)
        });

    for result in walker.build() {
        let entry = result
            .map_err(|error| WarpError::new(format!("walk workspace package sources: {error}")))?;
        if entry.depth() == 0 {
            continue;
        }
        let path = entry.path();
        let kind = entry.file_type().ok_or_else(|| {
            WarpError::new(format!(
                "could not inspect workspace source path: {}",
                path.display()
            ))
        })?;
        if kind.is_symlink() {
            return Err(WarpError::new(format!(
                "workspace package contains a symbolic link that Warp will not follow: {}",
                path.display()
            )));
        }
        if kind.is_file() {
            files.push(
                path.strip_prefix(root)
                    .map_err(|_| WarpError::new("workspace source escaped its package root"))?
                    .to_path_buf(),
            );
        }
    }
    Ok(())
}

fn hash_tree_into(
    hasher: &mut blake3::Hasher,
    root: &Path,
    excluded_directories: &[String],
    hash_pool: &HashPool,
) -> Result<()> {
    let mut files = Vec::new();
    collect_files(root, &mut files, excluded_directories, false)?;
    files.sort();
    let paths = files
        .iter()
        .map(|relative| root.join(relative))
        .collect::<Vec<_>>();
    hash_paths_into(hasher, root, &paths, hash_pool)
}

fn hash_paths_into(
    hasher: &mut blake3::Hasher,
    root: &Path,
    paths: &[PathBuf],
    hash_pool: &HashPool,
) -> Result<()> {
    let digests = hash_pool.hash_files(paths)?;
    for (path, digest) in paths.iter().zip(&digests) {
        hash_path_digest_into(hasher, path, root, digest)?;
    }
    Ok(())
}

fn hash_path_digest_into(
    hasher: &mut blake3::Hasher,
    path: &Path,
    root: &Path,
    digest: &str,
) -> Result<()> {
    let relative = path
        .strip_prefix(root)
        .map_err(|_| WarpError::new("workspace fingerprint input escaped its root"))?;
    write_field(
        hasher,
        relative.to_string_lossy().replace('\\', "/").as_bytes(),
    );
    let length = fs::metadata(path)
        .map_err(|error| WarpError::io("inspect workspace fingerprint input", error))?
        .len()
        .to_le_bytes();
    write_field(hasher, &length);
    write_field(hasher, digest.as_bytes());
    Ok(())
}

fn path_argument(path: &Path) -> Result<String> {
    path.to_str()
        .map(str::to_owned)
        .ok_or_else(|| WarpError::new(format!("rustc path is not valid UTF-8: {}", path.display())))
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
    compilation_target: &CompilationTargetTriple,
) -> Result<(PathBuf, String)> {
    let crate_name = normalize_crate_name(target_name);
    let (filename, compiler_artifact_name) = match kind {
        TargetKind::Binary => {
            let suffix = binary_suffix(compilation_target);
            (
                format!("{target_name}{suffix}"),
                format!("{crate_name}{suffix}"),
            )
        }
        TargetKind::Library => {
            let name = format!("lib{crate_name}.rlib");
            (name.clone(), name)
        }
        TargetKind::ProcMacro => {
            let name = format!(
                "{}{}{}",
                platform::dynamic_library_prefix(&crate_name),
                crate_name,
                platform::dynamic_library_suffix()
            );
            (name.clone(), name)
        }
    };
    Ok((
        build_directory.join(package_relative).join(filename),
        compiler_artifact_name,
    ))
}

fn binary_suffix(compilation_target: &CompilationTargetTriple) -> &'static str {
    if compilation_target.as_str().contains("windows") {
        ".exe"
    } else if compilation_target.as_str().starts_with("wasm32-") {
        ".wasm"
    } else {
        ""
    }
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
            .unwrap_or("build-artifact"),
        std::process::id()
    ));
    fs::copy(source, &temporary).map_err(|error| WarpError::io("copy promoted binary", error))?;
    platform::mark_executable(&temporary)
        .map_err(|error| WarpError::io("set promoted binary permissions", error))?;
    platform::publish_file(&temporary, output)
        .map_err(|error| WarpError::io("publish promoted binary", error))
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
    fn labels_resolve_packages_and_individual_build_targets() {
        let id = "path+file:///example/demo#0.1.0".to_string();
        let workspace_root =
            std::env::temp_dir().join(format!("warp-labels-{}", std::process::id()));
        let package_root = workspace_root.join("crates/demo");
        fs::create_dir_all(package_root.join("src")).unwrap();
        fs::write(package_root.join("Cargo.toml"), "[package]").unwrap();
        let package = CargoPackage {
            id: id.clone(),
            name: "demo".into(),
            version: "0.1.0".into(),
            manifest_path: package_root.join("Cargo.toml"),
            edition: "2021".into(),
            targets: vec![CargoTarget {
                name: "demo-bin".into(),
                kind: vec!["bin".into()],
                src_path: package_root.join("src/main.rs"),
                required_features: Vec::new(),
            }],
        };
        let packages = HashMap::from([(id.clone(), package)]);
        let members = HashSet::from([id.clone()]);
        let workspace_root = workspace_root.canonicalize().unwrap();

        let target_selection = select_workspace_targets(
            &BuildScope::Labels(vec![Label::parse("demo::demo-bin").unwrap()]),
            &packages,
            &members,
            &workspace_root,
        )
        .unwrap();
        assert_eq!(
            target_selection.get(&id),
            Some(&Some(HashSet::from(["demo-bin".to_string()])))
        );

        let package_selection = select_workspace_targets(
            &BuildScope::Labels(vec![Label::parse("./crates/demo").unwrap()]),
            &packages,
            &members,
            &workspace_root,
        )
        .unwrap();
        assert_eq!(package_selection.get(&id), Some(&None));
        fs::remove_dir_all(workspace_root).unwrap();
    }

    #[test]
    fn memory_mapped_artifact_hash_matches_blake3_for_small_and_large_files() {
        let test_root = std::env::temp_dir().join(format!(
            "warp-hash-test-{}-{}",
            std::process::id(),
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        ));
        fs::create_dir_all(&test_root).unwrap();
        let hash_pool = HashPool::new().unwrap();

        for (name, bytes) in [
            ("small", vec![17; 64 * 1024]),
            ("large", vec![29; 2 * 1024 * 1024]),
        ] {
            let path = test_root.join(name);
            fs::write(&path, &bytes).unwrap();
            assert_eq!(
                hash_pool.hash_file(&path).unwrap(),
                blake3::hash(&bytes).to_hex().to_string()
            );
        }

        fs::remove_dir_all(test_root).unwrap();
    }

    #[test]
    fn source_snapshots_honor_nested_gitignore_rules_and_hard_exclusions() {
        let test_root = std::env::temp_dir().join(format!(
            "warp-gitignore-test-{}-{}",
            std::process::id(),
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        ));
        let source = test_root.join("source");
        let snapshot = test_root.join("snapshot");
        fs::create_dir_all(source.join("nested")).unwrap();
        fs::create_dir_all(source.join("ignored-dir")).unwrap();
        fs::create_dir_all(source.join("_build")).unwrap();
        fs::create_dir_all(source.join(".git/objects")).unwrap();
        fs::write(
            source.join(".gitignore"),
            "*.log\n!important.log\nignored-dir/*\n!ignored-dir/keep.rs\n_build/\n!_build/\n!_build/keep.rs\n",
        )
        .unwrap();
        fs::write(source.join("main.rs"), "fn main() {}\n").unwrap();
        fs::write(source.join(".secret"), "keep hidden inputs\n").unwrap();
        fs::write(source.join("ignored.log"), "ignored\n").unwrap();
        fs::write(source.join("important.log"), "included by negation\n").unwrap();
        fs::write(source.join("ignored-dir/drop.rs"), "ignored\n").unwrap();
        fs::write(source.join("ignored-dir/keep.rs"), "included by negation\n").unwrap();
        fs::write(source.join("nested/.gitignore"), "*.tmp\n!keep.tmp\n").unwrap();
        fs::write(source.join("nested/drop.tmp"), "ignored\n").unwrap();
        fs::write(source.join("nested/keep.tmp"), "included by negation\n").unwrap();
        fs::write(source.join("_build/keep.rs"), "always ignored\n").unwrap();
        fs::write(source.join(".git/objects/metadata"), "always ignored\n").unwrap();

        copy_and_fingerprint_tree(&source, &snapshot, &[], &HashPool::new().unwrap()).unwrap();

        assert!(snapshot.join(".gitignore").is_file());
        assert!(snapshot.join("main.rs").is_file());
        assert!(snapshot.join(".secret").is_file());
        assert!(snapshot.join("important.log").is_file());
        assert!(snapshot.join("ignored-dir/keep.rs").is_file());
        assert!(snapshot.join("nested/keep.tmp").is_file());
        assert!(!snapshot.join("ignored.log").exists());
        assert!(!snapshot.join("ignored-dir/drop.rs").exists());
        assert!(!snapshot.join("nested/drop.tmp").exists());
        assert!(!snapshot.join("_build/keep.rs").exists());
        assert!(!snapshot.join(".git").exists());

        fs::remove_dir_all(test_root).unwrap();
    }

    #[test]
    fn package_artifacts_mirror_workspace_paths() {
        let build_directory = Path::new("/workspace/_build/aarch64-apple-darwin/release");
        let package = Path::new("crates/warp-store");
        let target = crate::host_triple().unwrap();
        let (library, _) = artifact_output(
            TargetKind::Library,
            "warp-store",
            package,
            build_directory,
            &target,
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
