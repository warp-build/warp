use std::sync::Arc;
use structopt::StructOpt;
use warp_core::*;

#[derive(StructOpt, Debug, Clone)]
#[structopt(
    name = "clean",
    setting = structopt::clap::AppSettings::ColoredHelp,
    about = "cleans a target"
)]
pub struct CleanGoal {
    #[structopt(help = r"The target to evict from the cache.


NOTE that after cleaning it, we may need to recompile all the dependants of this target.

A path to a directory with a warp file, followed by a colon
and the name of the label to be cleaned.

Example: //my/library:shell
")]
    target: String,
}

impl CleanGoal {
    #[tracing::instrument(name = "CleanGoal::run", skip(_workspace, _event_channel))]
    pub async fn run(
        self,
        _workspace: Workspace,
        _event_channel: Arc<EventChannel>,
    ) -> Result<(), anyhow::Error> {
        let _ = self.target;
        /*

        let target: Label = self.target.into();
        debug!("Host: {}", guess_host_triple::guess_host_triple().unwrap());
        debug!("Target: {}", &target.to_string());

        let mut cache = LocalCache::new(&workspace);
        let mut warp = LocalWorker::from_workspace(workspace.clone());

        let name = if target.is_all() {
            "workspace".to_string()
        } else {
            target.to_string()
        };

        print!("🔨 Preparing {}...", &name);
        std::io::stdout().flush().unwrap();

        warp.prepare(&target).await?;

        let nodes = warp.compute_nodes().await?;

        if target.is_all() {
            for node in &nodes {
                if node.target.is_local() {
                    let sandbox = LocalSandbox::for_node(&workspace, node.clone());
                    sandbox.clear_sandbox().await?;
                    cache.evict(&node).await?;
                }
            }
        } else {
            for node in &nodes {
                if *node.target.label() == target {
                    let sandbox = LocalSandbox::for_node(&workspace, node.clone());
                    sandbox.clear_sandbox().await?;
                    cache.evict(&node).await?;
                }
            }
        }
        */

        Ok(())
    }
}
