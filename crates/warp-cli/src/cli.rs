use clap::{Args, Parser, Subcommand};
use std::path::PathBuf;
use std::time::Instant;
use warp_build_engine::{
    build, build_workspace, crate_name, host_target, rustc_output_filename, status_tag,
    stdout_status_tag, BuildOutcome, BuildProfile, BuildRequest, Edition, TargetTriple,
    WorkspaceRequest,
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
    /// Build a workspace, selected Cargo package, or standalone Rust source file.
    Build(BuildArgs),
}

#[derive(Debug, Args)]
struct BuildArgs {
    /// Build a Rust source file or a Cargo package directory/Cargo.toml. Without it, build the workspace.
    path: Option<PathBuf>,

    /// Promoted executable path. Defaults to _build/<target>/<profile>/bin/<name>.
    #[arg(long)]
    output: Option<PathBuf>,

    /// Override the per-user global content and action cache directory.
    #[arg(long)]
    cache: Option<PathBuf>,

    /// Rust target triple. Defaults to this machine's host target.
    #[arg(long)]
    target: Option<String>,

    /// Build profile.
    #[arg(long, default_value = "debug", value_parser = ["debug", "release"])]
    profile: String,

    /// Rust language edition for standalone source builds.
    #[arg(long, default_value = "2021", value_parser = ["2015", "2018", "2021", "2024"])]
    edition: String,
}

impl Cli {
    pub fn run(self) -> Result<String, String> {
        match self.command {
            Command::Build(args) => {
                let target = match args.target.as_deref() {
                    Some(target) => {
                        TargetTriple::parse(target).map_err(|error| error.to_string())?
                    }
                    None => host_target().map_err(|error| error.to_string())?,
                };
                let profile =
                    BuildProfile::parse(&args.profile).map_err(|error| error.to_string())?;

                match args.path {
                    Some(entry) if is_standalone_source(&entry) => run_standalone_build(
                        entry,
                        args.output,
                        args.cache,
                        args.edition,
                        target,
                        profile,
                    ),
                    selection => {
                        run_workspace_build(selection, args.output, args.cache, target, profile)
                    }
                }
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
    target: TargetTriple,
    profile: BuildProfile,
) -> Result<String, String> {
    let edition = Edition::parse(&edition).map_err(|error| error.to_string())?;
    let output = match output {
        Some(output) => output,
        None => default_standalone_output(&target, profile, &entry)?,
    };
    let label = crate_name(&entry).map_err(|error| error.to_string())?;
    let request = BuildRequest {
        entry,
        output,
        cache,
        edition,
        target,
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

fn run_workspace_build(
    selection: Option<PathBuf>,
    output: Option<PathBuf>,
    cache: Option<PathBuf>,
    target: TargetTriple,
    profile: BuildProfile,
) -> Result<String, String> {
    let root = std::env::current_dir()
        .map_err(|error| format!("cannot determine current directory: {error}"))?;
    let request = WorkspaceRequest {
        root,
        selection,
        output,
        cache,
        target,
        profile,
    };
    let report = build_workspace(&request).map_err(|error| error.to_string())?;
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

fn default_standalone_output(
    target: &TargetTriple,
    profile: BuildProfile,
    entry: &std::path::Path,
) -> Result<PathBuf, String> {
    let crate_name = crate_name(entry).map_err(|error| error.to_string())?;
    let output_name =
        rustc_output_filename(target, &crate_name, "bin").map_err(|error| error.to_string())?;
    let root = std::env::current_dir()
        .map_err(|error| format!("cannot determine current directory: {error}"))?;
    Ok(root
        .join("_build")
        .join(target.as_str())
        .join(profile.as_str())
        .join("bin")
        .join(output_name))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn clap_parses_default_bootstrap_target() {
        let cli = Cli::try_parse_from(["warp", "build"]).unwrap();
        assert!(matches!(cli.command, Command::Build(_)));
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
        let target = TargetTriple::parse("wasm32-wasip1").unwrap();
        let entry = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("src/cli.rs");
        let output = default_standalone_output(&target, BuildProfile::Release, &entry).unwrap();
        assert!(output.ends_with("_build/wasm32-wasip1/release/bin/cli.wasm"));
    }
}
