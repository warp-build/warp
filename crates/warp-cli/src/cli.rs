use clap::{Args, Parser, Subcommand};
use std::path::PathBuf;
use warp_build_engine::{
    build, build_workspace, BuildOutcome, BuildRequest, Edition, WorkspaceRequest,
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
    /// Build the Cargo workspace with Warp, or a standalone Rust source file if supplied.
    Build(BuildArgs),
}

#[derive(Debug, Args)]
struct BuildArgs {
    /// Optional standalone Rust entry point. Without it, Warp builds the current Cargo workspace.
    entry: Option<PathBuf>,

    /// Output executable path.
    #[arg(long, default_value = "_build/debug/warp")]
    output: PathBuf,

    /// Local content and action cache directory.
    #[arg(long, default_value = "_build/cache")]
    cache: PathBuf,

    /// Rust language edition for standalone source builds.
    #[arg(long, default_value = "2021", value_parser = ["2015", "2018", "2021", "2024"])]
    edition: String,
}

impl Cli {
    pub fn run(self) -> Result<String, String> {
        match self.command {
            Command::Build(args) => {
                if let Some(entry) = args.entry {
                    let edition =
                        Edition::parse(&args.edition).map_err(|error| error.to_string())?;
                    let request = BuildRequest {
                        entry,
                        output: args.output,
                        cache: args.cache,
                        edition,
                    };
                    let outcome = build(&request).map_err(|error| error.to_string())?;
                    let status = match outcome {
                        BuildOutcome::Compiled { key } => format!("built ({})", &key[7..19]),
                        BuildOutcome::CacheHit { key } => format!("cached ({})", &key[7..19]),
                    };
                    Ok(format!("{status}: {}", request.output.display()))
                } else {
                    let root = std::env::current_dir()
                        .map_err(|error| format!("cannot determine current directory: {error}"))?;
                    let request = WorkspaceRequest {
                        root,
                        output: args.output,
                        cache: args.cache,
                    };
                    let report = build_workspace(&request).map_err(|error| error.to_string())?;
                    let status = if report.built == 0 {
                        format!("cached {} targets", report.cached)
                    } else {
                        format!("built {} targets, cached {}", report.built, report.cached)
                    };
                    Ok(format!("{status}: {}", report.output.display()))
                }
            }
        }
    }
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
    fn clap_rejects_unknown_edition_and_missing_option_values() {
        assert!(Cli::try_parse_from(["warp", "build", "--edition", "2017"]).is_err());
        assert!(Cli::try_parse_from(["warp", "build", "--output"]).is_err());
    }
}
