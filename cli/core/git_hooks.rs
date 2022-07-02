use super::*;
use std::path::PathBuf;
use tokio::fs;

enum Hooks {
    PreReceive,
}

impl Hooks {
    fn filename(&self) -> PathBuf {
        match self {
            Hooks::PreReceive => PathBuf::from("pre-receive"),
        }
    }

    fn contents(&self) -> String {
        match self {
            Hooks::PreReceive => include_str!("./git_hooks/pre-receive").to_string(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct GitHooks {
    root: PathBuf,
}

impl GitHooks {
    pub fn from_workspace(workspace: &Workspace) -> GitHooks {
        GitHooks {
            root: workspace.paths.workspace_root.join(".git").join("hooks"),
        }
    }

    pub async fn ensure_installed(&self) -> Result<(), anyhow::Error> {
        self.ensure_hook_exists(Hooks::PreReceive).await
    }

    async fn ensure_hook_exists(&self, hook: Hooks) -> Result<(), anyhow::Error> {
        let path = self.root.join(hook.filename());
        if fs::File::open(&path).await.is_err() {
            fs::write(&path, hook.contents()).await?;
        }
        Ok(())
    }
}
