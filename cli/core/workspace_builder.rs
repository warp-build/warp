use super::*;
use std::path::PathBuf;
use tokio::fs;

pub struct WorkspaceBuilder {}

impl WorkspaceBuilder {
    #[tracing::instrument(name = "WorkspaceBuilder::build")]
    pub async fn build(
        cwd: PathBuf,
        home: Option<String>,
        user: Option<String>,
    ) -> Result<Workspace, anyhow::Error> {
        let abs_cwd = fs::canonicalize(&cwd).await.unwrap();
        let (root, workspace_file) = WorkspaceScanner::find_workspace_file(&abs_cwd).await?;
        let paths = WorkspacePaths::new(&root, home, user)?;
        let (local_rules, local_toolchains) = {
            let scanner = WorkspaceScanner::from_paths(&paths);
            (
                scanner.find_rules().await?,
                scanner.find_toolchains().await?,
            )
        };

        let workspace = WorkspaceParser::from_toml(
            toml::from_str(&fs::read_to_string(&workspace_file).await?)?,
            paths,
            &local_rules,
            &local_toolchains,
        )?;

        Ok(workspace)
    }
}
