use std::sync::Arc;
use structopt::StructOpt;
use warp_core::*;

#[derive(StructOpt, Debug, Clone)]
#[structopt(
    name = "info",
    setting = structopt::clap::AppSettings::ColoredHelp,
    about = "Information about a target"
)]
pub struct InfoGoal {
    #[structopt(help = r"The target to get information about.

A path to a directory with a warp file, followed by a colon
and the name of the label to be built.

Example: //my/library:shell
")]
    label: String,
}

impl InfoGoal {
    pub async fn run(
        self,
        _workspace: Workspace,
        _event_channel: Arc<EventChannel>,
    ) -> Result<(), anyhow::Error> {
        let _ = self.label;
        /*
            let label: Label = self.label.into();
            debug!("Host: {}", guess_host_triple::guess_host_triple().unwrap());
            debug!("Target: {}", &label.to_string());

            let mut warp = LocalWorker::from_workspace(workspace);

            let name = if label.is_all() {
                "workspace".to_string()
            } else {
                label.to_string()
            };

            print!("🔨 Preparing {}...", &name);
            std::io::stdout().flush().unwrap();

            warp.prepare(&label).await?;

            for node in &warp.compute_nodes().await? {
                if *node.label == label {
                    println!("");
                    println!("Target info:");
                    println!(" - Label: {}", name);
                    println!(" - Rule: {}", node.rule.name());
                    println!(" - Kind: {:?}", node.kind);
                    println!(" - Hash: {}", node.hash);
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
                    let find_node = |label| (&warp.dep_graph).find_node(&label).clone();
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
