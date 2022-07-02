use super::*;
use std::os::unix::fs::PermissionsExt;
use std::path::PathBuf;
use tokio::fs;
use tokio::io::AsyncWriteExt;

const PRE_COMMIT: &str = include_str!("./git_hooks/pre-commit");

enum Hooks {
    PreCommit,
}

impl Hooks {
    fn filename(&self) -> PathBuf {
        match self {
            Hooks::PreCommit => PathBuf::from("pre-commit"),
        }
    }

    fn contents(&self) -> &[u8] {
        match self {
            Hooks::PreCommit => PRE_COMMIT,
        }
        .as_bytes()
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
        self.ensure_hook_exists(Hooks::PreCommit).await
    }

    async fn ensure_hook_exists(&self, hook: Hooks) -> Result<(), anyhow::Error> {
        let path = self.root.join(hook.filename());
        let mut file = fs::File::create(&path).await?;
        file.write_all(hook.contents()).await?;
        let permissions = std::fs::Permissions::from_mode(0o777);
        file.set_permissions(permissions).await?;
        Ok(())
    }
}
