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
    #[structopt(
        name = "target",
        short = "t",
        long = "target",
        help = r#"The label of the target you are aliasing."#
    )]
    label: String,

    #[structopt(
        name = "name",
        short = "n",
        long = "name",
        help = r#"The name to use as an alias."#
    )]
    name: String,
}

impl AliasGoal {
    pub async fn run(
        self,
        mut workspace: Workspace,
        _event_channel: Arc<EventChannel>,
    ) -> Result<(), anyhow::Error> {
        workspace
            .aliases
            .insert(self.name.clone(), self.label.into());
        let file: WorkspaceFile = (&workspace).try_into()?;
        file.write(&workspace.paths.workspace_root).await?;
        Ok(())
    }
}
