use log::*;
use std::io::Write;
use structopt::StructOpt;
use zap_core::*;

#[derive(StructOpt, Debug, Clone)]
#[structopt(
    name = "clean",
    setting = structopt::clap::AppSettings::ColoredHelp,
    about = "cleans a target"
)]
pub struct CleanGoal {
    #[structopt(help = r"The target to evict from the cache.


NOTE that after cleaning it, we may need to recompile all the dependants of this target.

A path to a directory with a zap file, followed by a colon
and the name of the label to be built.

Example: //my/library:shell
")]
    target: String,
}

impl CleanGoal {
    pub async fn run(self, workspace: Workspace) -> Result<(), anyhow::Error> {
        let target: Label = self.target.into();
        debug!("Host: {}", guess_host_triple::guess_host_triple().unwrap());
        debug!("Target: {}", &target.to_string());

        let mut cache = LocalCache::new(&workspace);
        let mut zap = LocalWorker::from_workspace(workspace);

        let name = if target.is_all() {
            "workspace".to_string()
        } else {
            target.to_string()
        };

        print!("🔨 Preparing {}...", &name);
        std::io::stdout().flush().unwrap();

        zap.prepare(&target).await?;

        let node = zap.compute_node().await?;

        cache.evict(&node)?;

        Ok(())

    }
}