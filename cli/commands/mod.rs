mod bootstrap;
mod build;
mod run;
mod setup;
mod test;

pub use bootstrap::*;
pub use build::*;
pub use run::*;
pub use setup::*;
pub use test::*;

use structopt::StructOpt;
use warp_core::Config;

#[derive(Default, Debug, Clone, Copy, StructOpt)]
pub struct Flags {
    #[structopt(
        help = r"Never access the network, but continue working if possible.",
        long = "offline"
    )]
    pub(crate) offline: Option<bool>,

    #[structopt(
        help = r"The amount of workers to use to execute any necessary build tasks.",
        short = "w",
        long = "max-localworkers"
    )]
    pub(crate) max_local_workers: Option<usize>,
}

impl From<Flags> for Config {
    fn from(flags: Flags) -> Self {
        Config::builder()
            .offline(flags.offline.unwrap_or(false))
            .max_local_workers(flags.max_local_workers.unwrap_or_else(num_cpus::get))
            .build()
            .unwrap()
    }
}
