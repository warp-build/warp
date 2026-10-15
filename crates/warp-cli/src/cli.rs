use clap::{Args, Parser, Subcommand};
use serde::Deserialize;
use std::fs;
use std::path::PathBuf;
use std::time::Instant;
use warp_build_engine::{
    build, build_workspace, crate_name, host_triple, rustc_output_filename, status_tag,
    stdout_status_tag, BuildClientConfig, BuildIntent, BuildOutcome, BuildProfile, BuildRequest,
    BuildScope, CompilationTargetTriple, Edition, Label, RustToolchainChannel,
};

#[derive(Debug, Parser)]
#[command(
    name = "warp",
    version,
    about = "A programmable, incremental build system"
)]
pub struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Debug, Subcommand)]
enum Command {
    /// Build the workspace or selected build targets.
    Build(BuildArgs),
}

#[derive(Debug, Args)]
struct BuildArgs {
    /// Optional target labels; omit to build every discoverable Rust and Bun target in the workspace.
    /// Labels may be package paths, Cargo package names, or package::target names.
    targets: Vec<String>,

    /// Promoted executable path. Defaults to the selected client-configured binary path.
    #[arg(long)]
    output: Option<PathBuf>,

    /// Binary target to promote instead of the client-configured default.
    #[arg(long)]
    bin: Option<String>,

    /// Override the per-user global content and action cache directory.
    #[arg(long)]
    cache: Option<PathBuf>,

    /// Compilation target triple (or configured platform alias). Defaults to the host triple.
    #[arg(short = 'x', long = "target-triple", alias = "target")]
    compilation_target: Option<String>,

    /// Build profile.
    #[arg(long, default_value = "debug", value_parser = ["debug", "release"])]
    profile: String,

    /// Rust language edition for standalone source builds.
    #[arg(long, default_value = "2021", value_parser = ["2015", "2018", "2021", "2024"])]
    edition: String,

    /// Show build graph planning details.
    #[arg(short, long)]
    verbose: bool,
}

impl Cli {
    pub fn run(self) -> Result<String, String> {
        match self.command {
            Command::Build(args) => {
                let BuildArgs {
                    targets,
                    output,
                    bin,
                    cache,
                    compilation_target,
                    profile,
                    edition,
                    verbose,
                } = args;
                let root = std::env::current_dir()
                    .map_err(|error| format!("cannot determine current directory: {error}"))?;
                let toolchains = load_toolchain_settings(&root)?;
                let compilation_target =
                    resolve_compilation_target(compilation_target.as_deref(), &toolchains)?;
                let profile = BuildProfile::parse(&profile).map_err(|error| error.to_string())?;
                let labels = targets
                    .into_iter()
                    .map(Label::parse)
                    .collect::<Result<Vec<_>, _>>()
                    .map_err(|error| error.to_string())?;

                if labels.len() == 1
                    && is_standalone_source(PathBuf::from(labels[0].as_str()).as_path())
                {
                    if bin.is_some() {
                        return Err("--bin applies only to Cargo workspace builds".into());
                    }
                    return run_standalone_build(
                        PathBuf::from(labels[0].as_str()),
                        output,
                        cache,
                        edition,
                        compilation_target,
                        toolchains.rust_channel,
                        profile,
                    );
                }
                run_workspace_build(WorkspaceBuildOptions {
                    labels,
                    output,
                    cache,
                    compilation_target,
                    rust_toolchain: toolchains.rust_channel,
                    profile,
                    verbose,
                    binary_override: bin,
                })
            }
        }
    }
}

fn is_standalone_source(path: &std::path::Path) -> bool {
    path.extension().and_then(|ext| ext.to_str()) == Some("rs")
}

fn run_standalone_build(
    entry: PathBuf,
    output: Option<PathBuf>,
    cache: Option<PathBuf>,
    edition: String,
    compilation_target: CompilationTargetTriple,
    rust_toolchain: Option<RustToolchainChannel>,
    profile: BuildProfile,
) -> Result<String, String> {
    let edition = Edition::parse(&edition).map_err(|error| error.to_string())?;
    let output = match output {
        Some(output) => output,
        None => default_standalone_output(&compilation_target, profile, &entry)?,
    };
    let label = crate_name(&entry).map_err(|error| error.to_string())?;
    let request = BuildRequest {
        entry,
        output,
        cache,
        edition,
        compilation_target,
        rust_toolchain,
        profile,
    };
    let started = Instant::now();
    eprintln!("Building {label}");
    let outcome = match build(&request) {
        Ok(outcome) => outcome,
        Err(error) => {
            let detail = error.to_string();
            eprintln!(
                "{} {label} {:.2}s {}",
                status_tag("ERR", false),
                started.elapsed().as_secs_f64(),
                detail.lines().next().unwrap_or(&detail)
            );
            eprintln!(
                "{} 0 targets in {:.2}s (failed)",
                status_tag("FINISHED", false),
                started.elapsed().as_secs_f64()
            );
            return Err(detail);
        }
    };
    match outcome {
        BuildOutcome::Compiled { .. } => eprintln!(
            "{} {label} {:.2}s",
            status_tag("OK", true),
            started.elapsed().as_secs_f64()
        ),
        BuildOutcome::CacheHit { .. } => eprintln!(
            "{} {label} {:.2}s (cached)",
            status_tag("OK", true),
            started.elapsed().as_secs_f64()
        ),
    }
    Ok(format!(
        "{} 1 targets in {:.2}s: {}",
        stdout_status_tag("FINISHED", true),
        started.elapsed().as_secs_f64(),
        request.output.display()
    ))
}

struct WorkspaceBuildOptions {
    labels: Vec<Label>,
    output: Option<PathBuf>,
    cache: Option<PathBuf>,
    compilation_target: CompilationTargetTriple,
    rust_toolchain: Option<RustToolchainChannel>,
    profile: BuildProfile,
    verbose: bool,
    binary_override: Option<String>,
}

fn run_workspace_build(options: WorkspaceBuildOptions) -> Result<String, String> {
    let WorkspaceBuildOptions {
        labels,
        output,
        cache,
        compilation_target,
        rust_toolchain,
        profile,
        verbose,
        binary_override,
    } = options;
    let root = std::env::current_dir()
        .map_err(|error| format!("cannot determine current directory: {error}"))?;
    let mut client = load_client_config(&root)?;
    if let Some(binary_override) = binary_override {
        client.default_binary =
            Some(Label::parse(binary_override).map_err(|error| error.to_string())?);
    }
    let scope = if labels.is_empty() {
        BuildScope::All
    } else {
        BuildScope::Labels(labels)
    };
    let intent = BuildIntent {
        root,
        scope,
        output,
        cache,
        compilation_target,
        rust_toolchain,
        profile,
        verbose,
        client,
    };
    let report = build_workspace(&intent).map_err(|error| error.to_string())?;
    let status = format!(
        "{} {} targets in {:.2}s ({} built, {} cached)",
        stdout_status_tag("FINISHED", true),
        report.targets,
        report.elapsed.as_secs_f64(),
        report.built,
        report.cached
    );
    Ok(match report.output {
        Some(output) => format!("{status}: {}", output.display()),
        None => status,
    })
}

#[derive(Default, Deserialize)]
#[serde(default)]
struct ClientConfigFile {
    build: ClientBuildConfig,
}

#[derive(Default, Deserialize)]
#[serde(default, rename_all = "kebab-case")]
struct ClientBuildConfig {
    default_binary: Option<String>,
    exclude_directories: Vec<String>,
}

#[derive(Default, Deserialize)]
#[serde(default)]
struct ToolchainsFile {
    toolchains: ToolchainsSection,
}

#[derive(Default, Deserialize)]
#[serde(default)]
struct ToolchainsSection {
    targets: Vec<String>,
    rust: Option<RustToolchainFile>,
}

#[derive(Default, Deserialize)]
#[serde(default, rename_all = "kebab-case")]
struct RustToolchainFile {
    channel: Option<String>,
}

#[derive(Default)]
struct ToolchainSettings {
    targets: Vec<CompilationTargetTriple>,
    rust_channel: Option<RustToolchainChannel>,
}

fn load_toolchain_settings(root: &std::path::Path) -> Result<ToolchainSettings, String> {
    let path = root.join("warp-toolchains.toml");
    let source = match fs::read_to_string(&path) {
        Ok(source) => source,
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => {
            return Ok(ToolchainSettings::default());
        }
        Err(error) => return Err(format!("read {}: {error}", path.display())),
    };
    let file: ToolchainsFile =
        toml::from_str(&source).map_err(|error| format!("parse {}: {error}", path.display()))?;
    let mut targets = Vec::new();
    for target in file.toolchains.targets {
        let target = CompilationTargetTriple::parse(&target)
            .map_err(|error| format!("parse {}: {error}", path.display()))?;
        if targets.contains(&target) {
            return Err(format!(
                "parse {}: duplicate compilation target triple {target}",
                path.display()
            ));
        }
        targets.push(target);
    }
    let rust_channel = file
        .toolchains
        .rust
        .and_then(|rust| rust.channel)
        .map(RustToolchainChannel::parse)
        .transpose()
        .map_err(|error| format!("parse {}: {error}", path.display()))?;
    Ok(ToolchainSettings {
        targets,
        rust_channel,
    })
}

fn resolve_compilation_target(
    requested: Option<&str>,
    settings: &ToolchainSettings,
) -> Result<CompilationTargetTriple, String> {
    let host = host_triple().map_err(|error| error.to_string())?;
    let Some(requested) = requested else {
        return Ok(host);
    };
    if let Ok(target) = CompilationTargetTriple::parse(requested) {
        if !settings.targets.is_empty() && !settings.targets.contains(&target) {
            return Err(format!(
                "compilation target {target} is not listed in warp-toolchains.toml"
            ));
        }
        return Ok(target);
    }

    let family = match requested.to_ascii_lowercase().as_str() {
        "macos" | "mac" | "darwin" => "apple-darwin",
        "linux" => "linux",
        "windows" | "win" => "windows",
        _ => return Err(format!("unknown compilation target alias {requested:?}")),
    };
    let candidates = settings
        .targets
        .iter()
        .filter(|target| target.as_str().contains(family))
        .collect::<Vec<_>>();
    let host_architecture = host.as_str().split('-').next().unwrap_or_default();
    let host_match = candidates.iter().copied().find(|candidate| {
        candidate
            .as_str()
            .starts_with(&format!("{host_architecture}-"))
    });
    let selected = match (host_match, candidates.as_slice()) {
        (Some(target), _) => target,
        (None, [target]) => *target,
        (None, []) => {
            return Err(format!(
                "no {requested} compilation target is configured in warp-toolchains.toml"
            ));
        }
        (None, _) => {
            return Err(format!(
                "compilation target alias {requested:?} is ambiguous; pass a full triple"
            ));
        }
    };
    Ok(selected.clone())
}

fn load_client_config(root: &std::path::Path) -> Result<BuildClientConfig, String> {
    let path = root.join("warp.toml");
    let source = match fs::read_to_string(&path) {
        Ok(source) => source,
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => {
            return Ok(BuildClientConfig::default());
        }
        Err(error) => return Err(format!("read {}: {error}", path.display())),
    };
    let config: ClientConfigFile =
        toml::from_str(&source).map_err(|error| format!("parse {}: {error}", path.display()))?;
    let default_binary = config
        .build
        .default_binary
        .map(Label::parse)
        .transpose()
        .map_err(|error| format!("parse {}: {error}", path.display()))?;
    Ok(BuildClientConfig {
        default_binary,
        excluded_directories: config.build.exclude_directories,
    })
}

fn default_standalone_output(
    compilation_target: &CompilationTargetTriple,
    profile: BuildProfile,
    entry: &std::path::Path,
) -> Result<PathBuf, String> {
    let crate_name = crate_name(entry).map_err(|error| error.to_string())?;
    let output_name = rustc_output_filename(compilation_target, &crate_name, "bin")
        .map_err(|error| error.to_string())?;
    let root = std::env::current_dir()
        .map_err(|error| format!("cannot determine current directory: {error}"))?;
    Ok(root
        .join("_build")
        .join(compilation_target.as_str())
        .join(profile.as_str())
        .join("bin")
        .join(output_name))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn clap_parses_default_build_and_multiple_labels_with_target_alias() {
        let cli = Cli::try_parse_from(["warp", "build"]).unwrap();
        assert!(matches!(cli.command, Command::Build(_)));

        let cli = Cli::try_parse_from([
            "warp",
            "build",
            "./crates/warp-rules",
            "./crates/warp-cli",
            "-x",
            "macos",
        ])
        .unwrap();
        let Command::Build(args) = cli.command;
        assert_eq!(
            args.targets,
            vec![
                "./crates/warp-rules".to_string(),
                "./crates/warp-cli".to_string()
            ]
        );
        assert_eq!(args.compilation_target.as_deref(), Some("macos"));

        let settings = ToolchainSettings {
            targets: ["aarch64-apple-darwin", "x86_64-apple-darwin"]
                .into_iter()
                .map(|target| CompilationTargetTriple::parse(target).unwrap())
                .collect(),
            rust_channel: None,
        };
        let host_arch = host_triple().unwrap();
        let resolved = resolve_compilation_target(Some("macos"), &settings).unwrap();
        assert!(resolved
            .as_str()
            .starts_with(host_arch.as_str().split('-').next().unwrap()));
    }

    #[test]
    fn clap_rejects_unknown_profile_edition_and_missing_option_values() {
        assert!(Cli::try_parse_from(["warp", "build", "--edition", "2017"]).is_err());
        assert!(Cli::try_parse_from(["warp", "build", "--profile", "fast"]).is_err());
        assert!(Cli::try_parse_from(["warp", "build", "--output"]).is_err());
        assert!(Cli::try_parse_from(["warp", "build", "--target"]).is_err());
    }

    #[test]
    fn output_path_mirrors_target_and_profile() {
        let target = CompilationTargetTriple::parse("wasm32-wasip1").unwrap();
        let entry = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("src/cli.rs");
        let output = default_standalone_output(&target, BuildProfile::Release, &entry).unwrap();
        assert!(output.ends_with("_build/wasm32-wasip1/release/bin/cli.wasm"));
    }
}
