use super::*;
use std::path::PathBuf;
use tracing::*;

pub struct WorkspaceBuilder {}

impl WorkspaceBuilder {
    #[tracing::instrument(name = "WorkspaceBuilder::build")]
    pub fn build(
        cwd: PathBuf,
        home: Option<String>,
        user: Option<String>,
    ) -> Result<Workspace, anyhow::Error> {
        let abs_cwd = std::fs::canonicalize(&cwd).unwrap();
        let (root, workspace_file) = WorkspaceScanner::find_workspace_file(&abs_cwd)?;
        let paths = WorkspacePaths::new(&root, home, user)?;
        let (local_rules, local_toolchains) = {
            let scanner = WorkspaceScanner::from_paths(&paths);
            (scanner.find_rules()?, scanner.find_toolchains()?)
        };

        let workspace = WorkspaceParser::from_toml(
            toml::from_str(&std::fs::read_to_string(&workspace_file)?)?,
            paths,
            &local_rules,
            &local_toolchains,
        )?;

        Ok(workspace)
    }
}
