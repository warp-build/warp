use anyhow::*;
use std::path::PathBuf;
use structopt::StructOpt;
use warp_core::*;

#[derive(StructOpt, Debug, Clone)]
#[structopt(
    name = "targets",
    setting = structopt::clap::AppSettings::ColoredHelp,
    about = "managing targets"
)]
pub struct TargetGoal {
    #[structopt(subcommand, help = "the command to run")]
    cmd: Action,
}

#[derive(StructOpt, Debug, Clone)]
#[structopt(
    setting = structopt::clap::AppSettings::ColoredHelp,
)]
enum Action {
    #[structopt(help = r"List all the workspace targets")]
    List,
}

impl TargetGoal {
    pub async fn run(self, config: WarpConfig) -> Result<(), anyhow::Error> {
        let mut warp = WarpWorker::new(config)?;
        warp.load(&PathBuf::from(&".")).await?;
        warp.build_dep_graph()?;

        match self.cmd {
            Action::List => self.list_targets(&mut warp),
        }
    }

    fn list_targets(&self, warp: &mut WarpWorker) -> Result<(), anyhow::Error> {
        let dep_graph = &mut warp.dep_graph;
        let mut targets = dep_graph.target_names();
        targets.sort();
        for target in targets {
            println!("{}", target);
        }
        Ok(())
    }
}
