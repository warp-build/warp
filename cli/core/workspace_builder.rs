use super::*;
use std::path::PathBuf;
use tokio::fs;

pub struct WorkspaceBuilder {}

impl WorkspaceBuilder {
    #[tracing::instrument(name = "WorkspaceBuilder::build")]
    pub async fn build(
        cwd: PathBuf,
        home: Option<String>,
        user: String,
    ) -> Result<Workspace, anyhow::Error> {
        let abs_cwd = fs::canonicalize(&cwd).await.unwrap();
        let (root, workspace_file) = WorkspaceScanner::find_workspace_file(&abs_cwd).await?;
        let paths = WorkspacePaths::new(&root, home, user.clone())?;
        let workspace = WorkspaceParser::from_toml(
            toml::from_str(&fs::read_to_string(&workspace_file).await?)?,
            user,
            paths,
        )?;

        /* NOTE(@ostera): disabled because it doesn't work
        let gitignore_patterns =
            WorkspaceParser::parse_gitignore_patterns(&workspace.paths.workspace_root).await;
        let workspace = workspace.with_gitignore_patterns(gitignore_patterns);
        */

        let (local_rules, local_toolchains) = {
            let scanner = WorkspaceScanner::from_workspace(&workspace);
            (
                scanner.find_rules().await?,
                scanner.find_toolchains().await?,
            )
        };

        if workspace.use_git_hooks {
            let githooks = GitHooks::from_workspace(&workspace);
            githooks.ensure_installed().await?;
        }

        Ok(workspace
            .with_rules(&local_rules)
            .with_toolchains(&local_toolchains))
    }
}
