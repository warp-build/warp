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
use url::Url;
use warp_core::Config;

#[derive(Default, Debug, Clone, StructOpt)]
pub struct Flags {
    #[structopt(
        help = r"Print the hashes of the artifacts that were built.",
        long = "print-hashes"
    )]
    pub(crate) print_hashes: bool,

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

    #[structopt(long = "public-store-metadata-url")]
    pub(crate) public_store_metadata_url: Option<Url>,

    #[structopt(long = "use-db")]
    pub(crate) use_db: bool,

    #[structopt(long = "force-redownload")]
    pub(crate) force_redownload: bool,
}

impl From<Flags> for Config {
    fn from(flags: Flags) -> Self {
        let mut config = Config::builder();

        config
            .offline(flags.offline.unwrap_or(false))
            .enable_code_database(flags.use_db)
            .max_local_workers(flags.max_local_workers.unwrap_or_else(num_cpus::get))
            .force_redownload(flags.force_redownload);

        if let Some(url) = flags.public_store_metadata_url {
            config.public_store_metadata_url(url);
        }

        config.build().unwrap()
    }
}
