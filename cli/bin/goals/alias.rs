use std::sync::Arc;
use structopt::StructOpt;
use warp_core::*;

#[derive(StructOpt, Debug, Clone)]
#[structopt(
    name = "alias",
    setting = structopt::clap::AppSettings::ColoredHelp,
    about = "managing targets"
)]
pub struct AliasGoal {
    target: String,
    alias: String,
}

impl AliasGoal {
    pub async fn run(
        self,
        _workspace: Workspace,
        _event_channel: Arc<EventChannel>,
    ) -> Result<(), anyhow::Error> {
        let _target: Label = self.target.into();
        // workspace.write_alias(self.alias, target).await?;
        Ok(())
    }
}
