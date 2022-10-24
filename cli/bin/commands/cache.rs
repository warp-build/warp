use anyhow::Context;
use std::path::PathBuf;
use structopt::StructOpt;
use warp_core::*;

#[derive(StructOpt, Debug, Clone)]
#[structopt(
    name = "cache",
    setting = structopt::clap::AppSettings::ColoredHelp,
    about = "manage the global cache",
)]
pub enum CacheCommand {
    #[structopt(help = r"Cleans the entire global cache.

NOTE: if this fixes your build, this is a bug. Please report it!
")]
    Purge,

    #[structopt(help = r"Clear a single target.

NOTE: if this fixes your build, this is a bug. Please report it!
")]
    Clear {
        #[structopt(help = r"The target to clean.

Must be a single target.
")]
        target: String,
    },
}

impl CacheCommand {
    pub async fn run(self, config: WarpConfig) -> Result<(), anyhow::Error> {
        match self {
            CacheCommand::Purge => {
                std::fs::remove_dir_all(config.archive_root)
                    .context("Could not remove entire Warp cache")?;
                std::fs::remove_dir_all(config.cache_root)
                    .context("Could not remove entire Warp cache")
            }
            CacheCommand::Clear { target } => {
                let target = workspace.aliases.handle_target(self.target);
                let mut warp = WarpWorker::new(config)?;
                warp.load(&PathBuf::from(&".")).await?;
                warp.build_dep_graph()?;
                let dep_graph = &mut warp.dep_graph.scoped(&target)?.seal(
                    &warp.action_map,
                    &warp.output_map,
                    &mut warp.bs_ctx,
                )?;

                let node = dep_graph.find_node(&target).context(format!(
                    "Target {} was not found in the Build Graph",
                    target.to_string()
                ))?;

                let cache_entry = warp.config.cache_root.join(node.hash());

                std::fs::remove_dir_all(&cache_entry).context(format!(
                    "Could not remove cached target: {}",
                    target.to_string()
                ))
            }
        }
    }
}
