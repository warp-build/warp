mod build;
mod info;
mod init;
mod lift;
mod new;
mod run;
mod setup;
mod shell;
mod test;

pub use build::*;
pub use info::*;
pub use init::*;
pub use lift::*;
pub use new::*;
pub use run::*;
pub use setup::*;
pub use shell::*;
pub use test::*;

use structopt::StructOpt;
use warp_core::BuildOpts;

#[derive(Default, Debug, Clone, Copy, StructOpt)]
pub struct Flags {
    #[structopt(
        help = r"The amount of workers to use to execute any necessary build tasks.",
        short = "w",
        long = "max-workers"
    )]
    max_workers: Option<usize>,

    #[structopt(
        help = r"show all the cache hit entries in the build outputs.",
        long = "show-cache-hits"
    )]
    show_cache_hits: bool,

    #[structopt(
        help = r"EXPERIMENTAL: trace target execution to detect missing inputs",
        long = "experimental-runtime-input-detection"
    )]
    experimental_runtime_input_detection: bool,

    #[structopt(
        help = r"EXPERIMENTAL: stream stderr/stdout of analyzer services",
        long = "experimental-stream-analyzer-outputs"
    )]
    experimental_stream_analyzer_outputs: bool,

    #[structopt(
        help = r"EXPERIMENTAL: ignore the cache and always rebuild",
        long = "experimental-force-rebuild"
    )]
    experimental_force_rebuild: bool,

    #[structopt(
        help = r"EXPERIMENTAL: load all the files instead of the build files",
        long = "experimental-file-mode"
    )]
    experimental_file_mode: bool,
}

impl Flags {
    pub fn into_build_opts(self) -> BuildOpts {
        self.into()
    }
}

impl From<Flags> for BuildOpts {
    fn from(flags: Flags) -> Self {
        BuildOpts {
            concurrency_limit: flags.max_workers.unwrap_or_else(num_cpus::get),
            experimental_file_mode: flags.experimental_file_mode,
            experimental_force_rebuild: flags.experimental_force_rebuild,
            experimental_stream_analyzer_outputs: flags.experimental_stream_analyzer_outputs,
            experimental_runtime_input_detection: flags.experimental_runtime_input_detection,
            ..Default::default()
        }
    }
}
