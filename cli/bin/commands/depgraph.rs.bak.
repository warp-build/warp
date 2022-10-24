use anyhow::*;
use std::path::PathBuf;
use structopt::StructOpt;
use warp_core::*;

#[derive(StructOpt, Debug, Clone)]
#[structopt(
    name = "dep-graph",
    setting = structopt::clap::AppSettings::ColoredHelp,
    about = "manage the dependency graph"
)]
pub struct DepGraphGoal {
    #[structopt(subcommand, help = "the command to run")]
    cmd: Action,
}

#[derive(StructOpt, Debug, Clone)]
#[structopt(
    setting = structopt::clap::AppSettings::ColoredHelp,
)]
enum Action {
    Print {
        #[structopt(help = r"Print the scoped in GraphViz format.
    ")]
        target: String,
    },
}

impl DepGraphGoal {
    #[tracing::instrument(name="DepGraphGoal::run")]
    pub async fn run(self, config: WarpConfig) -> Result<(), anyhow::Error> {
        let mut warp = WarpWorker::new(config)?;
        warp.load(&PathBuf::from(&".")).await?;
        warp.build_dep_graph()?;

        match self.cmd {
            Action::Print { ref target } => self.print(&target, &mut warp),
        }
    }

    #[tracing::instrument(name="DepGraphGoal::print")]
    fn print(&self, target: &str, warp: &mut WarpWorker) -> Result<(), anyhow::Error> {
        let label: Label = target.into();
        let dep_graph = &mut warp.dep_graph.scoped(&label)?.seal(
            &warp.action_map,
            &warp.output_map,
            &mut warp.bs_ctx,
        )?;

        println!("{}", dep_graph.to_graphviz());

        Ok(())
    }
}
