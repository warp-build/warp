use crate::action_cache::ActionCache;
use crate::error::{Result, WarpError};
use crate::fingerprint::rustc;
use crate::graph::{ActionPlan, GraphExecutor, Plan, TaskId, TaskProvider};
use serde::Deserialize;
use std::collections::{HashMap, HashSet};
use std::fs::{self, File};
use std::io::Read;
use std::num::NonZeroUsize;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;
use warp_store::{BlobDigest, Cas};

static NEXT_WORK: AtomicU64 = AtomicU64::new(0);

#[derive(Clone, Debug)]
pub struct WorkspaceRequest {
    pub root: PathBuf,
    pub output: PathBuf,
    pub cache: PathBuf,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct WorkspaceReport {
    pub output: PathBuf,
    pub built: usize,
    pub cached: usize,
}

pub fn build_workspace(request: &WorkspaceRequest) -> Result<WorkspaceReport> {
    let root = request
        .root
        .canonicalize()
        .map_err(|error| WarpError::io("resolve workspace root", error))?;
    let metadata = load_metadata(&root)?;
    let root = metadata.workspace_root.clone();

    let output_parent = output_parent(&request.output)?;
    let output_name = request
        .output
        .file_name()
        .ok_or_else(|| WarpError::new("workspace output path must name a file"))?;
    let main_output = output_parent.join(output_name);
    let dependency_output = output_parent.join("deps");
    fs::create_dir_all(&dependency_output)
        .map_err(|error| WarpError::io("create workspace output directory", error))?;

    let cache_root = absolute_directory(&request.cache, "create workspace cache")?;
    let action_cache = Arc::new(ActionCache::new(cache_root.join("actions"))?);
    let cas = Arc::new(
        Cas::new(cache_root.join("cas"))
            .map_err(|error| WarpError::new(format!("open content store: {error}")))?,
    );

    let work = WorkDirectory::create(&cache_root)?;
    let workspace =
        Workspace::from_metadata(metadata, &root, &work.path, &output_parent, &main_output)?;
    let (compiler, compiler_version) = rustc()?;
    let provider = WorkspaceProvider {
        workspace: Arc::new(workspace),
        compiler,
        compiler_version,
        action_cache,
        cas,
        built: AtomicU64::new(0),
        cached: AtomicU64::new(0),
    };

    let workers = std::thread::available_parallelism().unwrap_or(NonZeroUsize::MIN);
    let graph = GraphExecutor::new(workers);
    graph
        .execute(&provider, provider.workspace.roots.clone())
        .map_err(|error| WarpError::new(format!("workspace graph failed: {error}")))?;

    let built = provider.built.load(Ordering::Relaxed) as usize;
    let cached = provider.cached.load(Ordering::Relaxed) as usize;
    Ok(WorkspaceReport {
        output: main_output,
        built,
        cached,
    })
}

fn load_metadata(root: &Path) -> Result<CargoMetadata> {
    let output = Command::new("cargo")
        .current_dir(root)
        .args(["metadata", "--locked", "--format-version", "1"])
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
    target: Option<String>,
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
    crate_name: String,
    edition: String,
    features: Vec<String>,
    kind: TargetKind,
    dependencies: Vec<BuildDependency>,
    output: PathBuf,
}

#[derive(Clone)]
struct BuildDependency {
    alias: String,
    package_id: String,
    internal_task: Option<TaskId>,
    artifact: Option<PathBuf>,
}

struct Workspace {
    target_directory: PathBuf,
    dependency_directory: PathBuf,
    cargo_dependency_directory: PathBuf,
    native_directories: Vec<PathBuf>,
    global_fingerprint: String,
    targets: HashMap<TaskId, TargetBuild>,
    roots: Vec<TaskId>,
}

impl Workspace {
    fn from_metadata(
        metadata: CargoMetadata,
        root: &Path,
        work_root: &Path,
        output_directory: &Path,
        main_output: &Path,
    ) -> Result<Self> {
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

        let mut package_targets = HashMap::<String, Vec<(CargoTarget, TargetKind)>>::new();
        for package_id in &workspace_member_ids {
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
        for package_id in &workspace_member_ids {
            for (target, kind) in &package_targets[package_id] {
                let id = task_id(package_id, &target.name)?;
                if *kind == TargetKind::Library || *kind == TargetKind::ProcMacro {
                    task_by_package_library.insert(package_id.clone(), id.clone());
                }
                target_ids.insert((package_id.clone(), target.name.clone()), id.clone());
                roots.push(id);
            }
        }
        roots.sort();

        let mut external_artifacts = HashMap::<String, PathBuf>::new();
        let mut targets = HashMap::new();
        let mut external_packages = HashSet::new();
        for package_id in &workspace_member_ids {
            let package = packages.get(package_id).unwrap();
            let node = nodes.get(package_id).ok_or_else(|| {
                WarpError::new(format!(
                    "Cargo metadata omitted dependency information for {}",
                    package.name
                ))
            })?;
            for dependency in normal_dependencies(&node.deps)? {
                if !workspace_member_ids.contains(&dependency.pkg) {
                    external_packages.insert(dependency.pkg.clone());
                }
            }
        }
        for external_id in external_packages {
            let package = packages.get(&external_id).ok_or_else(|| {
                WarpError::new(format!("Cargo metadata omitted package {external_id}"))
            })?;
            let target = package
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
            let artifact = find_external_artifact(
                &metadata.target_directory,
                &package.name,
                &package.version,
                &target.name,
                target.kind.iter().any(|kind| kind == "proc-macro"),
                features,
            )?;
            external_artifacts.insert(external_id, artifact);
        }

        for package_id in &workspace_member_ids {
            let package = packages.get(package_id).unwrap();
            let package_root = package
                .manifest_path
                .parent()
                .ok_or_else(|| WarpError::new("Cargo manifest path has no parent"))?
                .to_path_buf();
            let node = nodes.get(package_id).unwrap();
            let dependencies = normal_dependencies(&node.deps)?
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
                let output = artifact_output(
                    *kind,
                    &target.name,
                    package_id,
                    output_directory,
                    main_output,
                );
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
                        crate_name: normalize_crate_name(&target.name),
                        edition: package.edition.clone(),
                        features: features.clone(),
                        kind: *kind,
                        dependencies: dependencies.clone(),
                        output,
                    },
                );
            }
        }

        let native_directories =
            collect_native_directories(&metadata.target_directory.join("debug/build"))?;
        let mut global_hasher = blake3::Hasher::new();
        write_field(&mut global_hasher, RUST_TARGET_RULE.as_bytes());
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
        let global_fingerprint = global_hasher.finalize().to_hex().to_string();
        let dependency_directory = output_directory.join("deps");
        let cargo_dependency_directory = metadata.target_directory.join("debug/deps");
        Ok(Self {
            target_directory: metadata.target_directory,
            dependency_directory,
            cargo_dependency_directory,
            native_directories,
            global_fingerprint,
            targets,
            roots,
        })
    }
}

#[derive(Clone)]
struct NormalDependency {
    name: String,
    pkg: String,
}

fn normal_dependencies(dependencies: &[CargoResolvedDependency]) -> Result<Vec<NormalDependency>> {
    dependencies
        .iter()
        .filter_map(|dependency| {
            let normal = dependency.dep_kinds.iter().any(|kind| kind.kind.is_none());
            if !normal {
                return None;
            }
            Some((dependency, dependency.dep_kinds.iter().find(|kind| kind.kind.is_none())))
        })
        .map(|(dependency, kind)| {
            if let Some(target) = kind.and_then(|kind| kind.target.as_deref()) {
                return Err(WarpError::new(format!(
                    "target-specific dependency {} ({target}) is not supported by this host-only bootstrap",
                    dependency.name
                )));
            }
            Ok(NormalDependency {
                name: dependency.name.clone(),
                pkg: dependency.pkg.clone(),
            })
        })
        .collect()
}

fn find_external_artifact(
    target_directory: &Path,
    package_name: &str,
    version: &str,
    crate_name: &str,
    proc_macro: bool,
    features: &[String],
) -> Result<PathBuf> {
    let dependencies = target_directory.join("debug/deps");
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
    let prefix = format!("lib{}-", normalize_crate_name(crate_name));
    let source_marker = format!("/{}-{}/", package_name, version);
    let mut candidates = Vec::new();
    for entry in fs::read_dir(&dependencies)
        .map_err(|error| WarpError::io("read Cargo dependency artifacts", error))?
    {
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
        let dep_info = dependencies.join(format!("{}-{hash}.d", normalize_crate_name(crate_name)));
        let Ok(dep_info) = fs::read_to_string(dep_info) else {
            continue;
        };
        if !dep_info.replace('\\', "/").contains(&source_marker) {
            continue;
        }

        let fingerprint = target_directory
            .join("debug/.fingerprint")
            .join(format!("{}-{hash}", normalize_crate_name(crate_name)))
            .join(format!("lib-{}.json", normalize_crate_name(crate_name)));
        let feature_match = fingerprint_features(&fingerprint)
            .map(|built_features| feature_match_score(features, &built_features))
            .unwrap_or_default();
        let modified = path
            .metadata()
            .and_then(|metadata| metadata.modified())
            .unwrap_or(std::time::SystemTime::UNIX_EPOCH);
        candidates.push((feature_match, modified, path));
    }

    candidates.sort_by(|left, right| left.0.cmp(&right.0).then(left.1.cmp(&right.1)));
    candidates
        .pop()
        .map(|(_, _, path)| path)
        .ok_or_else(|| {
            WarpError::new(format!(
                "no prebuilt Cargo artifact for {package_name} {version} ({crate_name}); run `cargo build` first"
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
        let target = self
            .workspace
            .targets
            .get(task)
            .ok_or_else(|| format!("unknown workspace task {task}"))?;
        let dependencies = self
            .resolve_dependencies(target)
            .map_err(|error| error.to_string())?;
        let key = target_action_key(
            &self.workspace.global_fingerprint,
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
                return Ok(());
            }
        }

        let work = WorkDirectory::create(&self.workspace.target_directory.join("warp-work"))
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
        Ok(())
    }
}

impl WorkspaceProvider {
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
    let mut compiler_arguments = vec![
        "--out-dir".to_string(),
        path_argument(output_directory)?,
        "--remap-path-prefix".to_string(),
        format!("{}=/warp-source", path_argument(&target.source_root)?),
        "--remap-path-prefix".to_string(),
        format!("{}=/warp-output", path_argument(output_directory)?),
        "-L".to_string(),
        format!(
            "dependency={}",
            path_argument(&workspace.cargo_dependency_directory)?
        ),
        "-L".to_string(),
        format!(
            "dependency={}",
            path_argument(&workspace.dependency_directory)?
        ),
    ];
    for directory in &workspace.native_directories {
        compiler_arguments.push("-L".to_string());
        compiler_arguments.push(format!("native={}", path_argument(directory)?));
    }
    if let Some(flags) = std::env::var_os("RUSTFLAGS") {
        compiler_arguments.extend(
            flags
                .to_string_lossy()
                .split_whitespace()
                .map(str::to_owned),
        );
    }

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

    let artifact = match target.kind {
        TargetKind::Library => output_directory.join(format!("lib{}.rlib", target.crate_name)),
        TargetKind::Binary => output_directory.join(executable_name(&target.crate_name)),
        TargetKind::ProcMacro => output_directory.join(format!(
            "{}{}",
            dynamic_library_prefix(&target.crate_name),
            dynamic_library_suffix(&target.crate_name)
        )),
    };
    if !artifact.is_file() {
        return Err(WarpError::new(format!(
            "rustc succeeded but did not produce expected artifact {}",
            artifact.display()
        )));
    }
    Ok(artifact)
}

fn target_action_key(
    global_fingerprint: &str,
    target: &TargetBuild,
    dependencies: &[(String, String, PathBuf)],
    compiler: &Path,
    compiler_version: &str,
) -> Result<String> {
    let mut hasher = blake3::Hasher::new();
    write_field(&mut hasher, b"warp-cargo-package-action-v1");
    write_field(&mut hasher, global_fingerprint.as_bytes());
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

fn output_parent(output: &Path) -> Result<PathBuf> {
    let parent = output
        .parent()
        .filter(|parent| !parent.as_os_str().is_empty())
        .unwrap_or_else(|| Path::new("."));
    fs::create_dir_all(parent)
        .map_err(|error| WarpError::io("create workspace output parent", error))?;
    parent
        .canonicalize()
        .map_err(|error| WarpError::io("resolve workspace output parent", error))
}

fn absolute_directory(path: &Path, action: &str) -> Result<PathBuf> {
    fs::create_dir_all(path).map_err(|error| WarpError::io(action, error))?;
    path.canonicalize()
        .map_err(|error| WarpError::io("resolve workspace cache directory", error))
}

fn artifact_output(
    kind: TargetKind,
    target_name: &str,
    package_id: &str,
    output_directory: &Path,
    main_output: &Path,
) -> PathBuf {
    match kind {
        TargetKind::Binary if target_name == "warp" => main_output.to_path_buf(),
        TargetKind::Binary => output_directory
            .join("bin")
            .join(executable_name(target_name)),
        TargetKind::Library | TargetKind::ProcMacro => output_directory.join("deps").join(format!(
            "lib{}-{}.{}",
            normalize_crate_name(target_name),
            short_hash(package_id),
            match kind {
                TargetKind::ProcMacro => dynamic_library_extension(),
                _ => "rlib",
            }
        )),
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

fn executable_name(name: &str) -> String {
    if cfg!(windows) {
        format!("{name}.exe")
    } else {
        name.to_string()
    }
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

fn dynamic_library_extension() -> &'static str {
    if cfg!(target_os = "macos") {
        "dylib"
    } else if cfg!(windows) {
        "dll"
    } else {
        "so"
    }
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
