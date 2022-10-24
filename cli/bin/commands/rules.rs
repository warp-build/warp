use anyhow::*;
use std::path::PathBuf;
use structopt::StructOpt;
use warp_core::*;

#[derive(StructOpt, Debug, Clone)]
#[structopt(
    name = "rule",
    setting = structopt::clap::AppSettings::ColoredHelp,
    about = "managing rules"
)]
pub enum RulesCommand {
    #[structopt(help = r"List all the local and global Warp rules.")]
    List,

    DumpActions {
        #[structopt(help = r"The target to dump actions for.

    This command is primarily useful to debug what actions will be taken to build
    a particular target.

    It will only print this targret's actions, and not any dependencies.
    ")]
        target: String,
    },

    DumpOutputs {
        #[structopt(help = r"The target to dump outputs for.

    This command is primarily useful to debug what outputs will be created when
    a particular target is built.

    It will only print this target's outputs.
    ")]
        target: String,
    },
}

impl RulesCommand {
    pub async fn run(self, config: WarpConfig) -> Result<(), anyhow::Error> {
        let mut warp = WarpWorker::new(config)?;
        warp.load(&PathBuf::from(&".")).await?;
        warp.build_dep_graph()?;

        match self {
            RulesCommand::List => self.list_rules(&mut warp),
            RulesCommand::DumpActions { ref target } => self.dump_actions(&target, &mut warp),
            RulesCommand::DumpOutputs { ref target } => self.dump_outputs(&target, &mut warp),
        }
    }

    fn list_rules(&self, warp: &mut WarpWorker) -> Result<(), anyhow::Error> {
        let rule_manager = warp.rule_manager();
        println!("Loaded Rules: ");
        let mut rules = rule_manager.read().unwrap().rules();
        rules.sort_by_key(|r| r.name().to_string());
        for rule in rules {
            println!("* {} @ {}", rule.mnemonic(), rule.name().to_string());
        }
        Ok(())
    }

    fn dump_outputs(&self, target: &str, warp: &mut WarpWorker) -> Result<(), anyhow::Error> {
        let label: Label = target.into();
        let dep_graph = &mut warp.dep_graph.scoped(&label)?.seal(
            &warp.action_map,
            &warp.output_map,
            &mut warp.bs_ctx,
        )?;

        for computed_target in dep_graph.targets() {
            for output in computed_target.outs {
                println!("{}", output.to_str().unwrap());
            }
        }

        Ok(())
    }

    fn dump_actions(&self, target: &str, warp: &mut WarpWorker) -> Result<(), anyhow::Error> {
        let label: Label = target.into();
        let dep_graph = &mut warp.dep_graph.scoped(&label)?.seal(
            &warp.action_map,
            &warp.output_map,
            &mut warp.bs_ctx,
        )?;

        for computed_target in dep_graph.targets() {
            for action in computed_target.actions {
                println!("{:?}", action);
            }
        }

        Ok(())
    }
}
