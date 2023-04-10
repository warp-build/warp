use std::path::PathBuf;

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
        help = r"Prints cache-hits. This can be very verbose.",
        long = "print-cache-hits"
    )]
    pub(crate) print_cache_hits: bool,

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

    #[structopt(
        help = r"Override where the Public Store fetches the artifact metadata.",
        long = "public-store-metadata-url"
    )]
    pub(crate) public_store_metadata_url: Option<Url>,

    #[structopt(
        help = r"Override where the Public Store fetches the artifact metadata.",
        long = "public-store-metadata-path"
    )]
    pub(crate) public_store_metadata_path: Option<PathBuf>,

    #[structopt(
        help = r"The location of the rule store in the current host.",
        long = "rule-store-root"
    )]
    pub(crate) rule_store_root: Option<PathBuf>,

    #[structopt(
        help = r"Skips database operations. This will typically regenerate Signatures and Plans that would otherwise be fetched from the database.",
        long = "skip-db"
    )]
    pub(crate) skip_db: bool,

    #[structopt(
        help = r"Avoid the downloading cache and redownload every external resource.
NOTE: this can result in a fresh build.
",
        long = "force-redownload"
    )]
    pub(crate) force_redownload: bool,

    #[structopt(
        help = r"Add a new directory to the rules resolution.",
        long = "add-rule-dir"
    )]
    pub(crate) add_rule_dir: Vec<PathBuf>,

    #[structopt(
        help = r"Change the directory in which Warp will start executing.

NOTE: Warp will automatically find the workspace directory by looking up from the invocation
directory until it finds a Warpfile.
    ",
        long = "invocation-dir"
    )]
    pub(crate) invocation_dir: Option<PathBuf>,

    #[structopt(help = r"Change the root of Warp's data folders.", long = "warp-root")]
    pub(crate) warp_root: Option<PathBuf>,
}

impl From<Flags> for Config {
    fn from(flags: Flags) -> Self {
        let mut config = Config::builder();

        config
            .offline(flags.offline.unwrap_or(false))
            .enable_code_database(!flags.skip_db)
            .max_local_workers(flags.max_local_workers.unwrap_or_else(num_cpus::get))
            .extra_rule_dirs(flags.add_rule_dir)
            .public_store_metadata_path(flags.public_store_metadata_path)
            .force_redownload(flags.force_redownload);

        if let Some(dir) = flags.invocation_dir {
            config.invocation_dir(dir);
        }

        if let Some(dir) = flags.rule_store_root {
            config.rule_store_root(dir);
        }

        if let Some(dir) = flags.warp_root {
            config.warp_root(dir);
        }

        if let Some(url) = flags.public_store_metadata_url {
            config.public_store_metadata_url(url);
        }

        config.build().unwrap()
    }
}
