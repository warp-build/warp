use std::collections::HashSet;
use std::io::Write;
use std::path::PathBuf;
use std::sync::Arc;
use structopt::StructOpt;
use tracing::*;
use zap_core::*;

#[derive(StructOpt, Debug, Clone)]
#[structopt(
    name = "info",
    setting = structopt::clap::AppSettings::ColoredHelp,
    about = "Information about a target"
)]
pub struct InfoGoal {
    #[structopt(help = r"The target to get information about.

A path to a directory with a zap file, followed by a colon
and the name of the label to be built.

Example: //my/library:shell
")]
    target: String,
}

impl InfoGoal {
    pub async fn run(
        self,
        workspace: Workspace,
        _event_channel: Arc<EventChannel>,
    ) -> Result<(), anyhow::Error> {
        /*
        let target: Label = self.target.into();
        debug!("Host: {}", guess_host_triple::guess_host_triple().unwrap());
        debug!("Target: {}", &target.to_string());

        let mut zap = LocalWorker::from_workspace(workspace);

        let name = if target.is_all() {
            "workspace".to_string()
        } else {
            target.to_string()
        };

        print!("🔨 Preparing {}...", &name);
        std::io::stdout().flush().unwrap();

        zap.prepare(&target).await?;

        for node in &zap.compute_nodes().await? {
            if *node.target.label() == target {
                println!("");
                println!("Target info:");
                println!(" - Label: {}", name);
                println!(" - Rule: {}", node.target.rule().name());
                println!(" - Kind: {:?}", node.target.kind());
                println!(" - Hash: {}", node.hash());
                println!(" - Sources: ");
                for src in node.srcs() {
                    println!("    - {}", src.to_str().unwrap());
                }
                println!(" - Outputs: ");
                let mut outs: Vec<PathBuf> = node
                    .outs()
                    .iter()
                    .cloned()
                    .collect::<HashSet<PathBuf>>()
                    .iter()
                    .cloned()
                    .collect::<Vec<PathBuf>>();
                outs.sort();
                for out in outs {
                    println!("    - {}", out.to_str().unwrap());
                }
                /*
                let find_node = |label| (&zap.dep_graph).find_node(&label).clone();
                println!(" - Dependencies: ");
                let mut deps: Vec<Dependency> = node
                    .transitive_deps(&find_node)?
                    .iter()
                    .cloned()
                    .collect::<HashSet<Dependency>>()
                    .iter()
                    .cloned()
                    .collect::<Vec<Dependency>>();
                deps.sort_by_key(|d| d.label.to_string());
                for dep in deps {
                    println!("    - {}", dep.label.to_string());
                }
                */
            }
        }
    */

        Ok(())
    }
}
