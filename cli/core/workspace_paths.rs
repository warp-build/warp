use crypto::digest::Digest;
use crypto::sha1::Sha1;
use std::path::Path;
use std::path::PathBuf;
use thiserror::*;

#[derive(Error, Debug)]
pub enum WorkspacePathsError {
    #[error("Could not create workspace path at {path:?} due to: {err:?}")]
    CouldNotCreateDir { path: PathBuf, err: std::io::Error },

    #[error("Could not symlink workspace path from {src:?} to {dst:?} due to: {err:?}")]
    CannotCreateSymlink {
        src: PathBuf,
        dst: PathBuf,
        err: std::io::Error,
    },
}

#[derive(Clone, Default, Debug)]
pub struct WorkspacePaths {
    /// The user currently using this workspace
    pub current_user: String,

    /// The name of the current workspace
    pub workspace_name: String,

    /// The location of the global cache
    pub global_cache_root: PathBuf,

    /// The location of the global sandbox
    pub global_sandbox_root: PathBuf,

    /// The location of global rules
    pub global_rules_root: PathBuf,

    /// The location of global toolchains
    pub global_toolchains_root: PathBuf,

    /// The location of the outputs for this workspace
    pub local_outputs_root: PathBuf,

    /// The location of the custom rules for this workspace
    pub local_rules_root: PathBuf,

    /// The location of the sandbox for this workspace
    pub local_sandbox_root: PathBuf,

    /// The location of the cache for this workspace
    pub local_cache_root: PathBuf,

    /// The location of the custom toolchains for this workspace
    pub local_toolchains_root: PathBuf,

    /// The path to this workspace
    pub workspace_root: PathBuf,

    /// The path to the symlinked outputs folder at the root of the workspace
    pub workspace_output_link: PathBuf,
}

impl WorkspacePaths {
    #[tracing::instrument(name = "WorkspacePaths::new")]
    pub fn new(
        workspace_root: &Path,
        home: Option<String>,
        current_user: String,
    ) -> Result<WorkspacePaths, WorkspacePathsError> {
        let workspace_name = {
            let mut hasher = Sha1::new();
            hasher.input_str(workspace_root.to_str().unwrap());
            format!(
                "{}-{}",
                workspace_root.file_name().unwrap().to_str().unwrap(),
                hasher.result_str()
            )
        };

        let warp_home = home
            .map(PathBuf::from)
            .unwrap_or_else(|| PathBuf::from("/warp"));

        let global_rules_root = warp_home.join("rules");
        let global_toolchains_root = warp_home.join("toolchains");

        let global_cache_root = warp_home.join("cache");
        let global_sandbox_root = warp_home.join("sandbox").join("global");

        let local_cache_root = warp_home.join("cache").join(&workspace_name);
        let local_sandbox_root = warp_home.join("sandbox").join(&workspace_name);
        let local_outputs_root = warp_home.join("outputs").join(&workspace_name);

        let workspace_output_link = workspace_root.join("warp-outputs");
        let local_warp_root = workspace_root.join(".warp");
        let local_rules_root = local_warp_root.join("rules");
        let local_toolchains_root = local_warp_root.join("toolchains");

        for path in &[
            &global_rules_root,
            &global_toolchains_root,
            &global_cache_root,
            &local_warp_root,
            &local_rules_root,
            &local_toolchains_root,
            &local_sandbox_root,
            &local_outputs_root,
        ] {
            std::fs::create_dir_all(&path).map_err(|err| {
                WorkspacePathsError::CouldNotCreateDir {
                    path: path.to_path_buf(),
                    err,
                }
            })?;
        }

        WorkspacePaths::setup_links(&local_outputs_root, &workspace_output_link)?;

        let paths = WorkspacePaths {
            current_user,
            workspace_name,
            global_cache_root,
            global_sandbox_root,
            global_rules_root,
            global_toolchains_root,
            local_outputs_root,
            local_rules_root,
            local_cache_root,
            local_sandbox_root,
            local_toolchains_root,
            workspace_output_link,
            workspace_root: workspace_root.to_path_buf(),
        };

        Ok(paths)
    }

    #[cfg(target_os = "windows")]
    #[tracing::instrument(name = "WorkspacePaths::setup_links")]
    fn setup_links(
        local_outputs_root: &PathBuf,
        workspace_output_link: &PathBuf,
    ) -> Result<(), WorkspacePathsError> {
        let _ = std::fs::remove_file(workspace_output_link);
        match std::os::windows::fs::symlink_dir(local_outputs_root.clone(), workspace_output_link) {
            Ok(_) => Ok(()),
            Err(err) if err.kind() == std::io::ErrorKind::AlreadyExists => Ok(()),
            Err(err) => Err(WorkspacePathsError::CannotCreateSymlink {
                src: local_outputs_root.clone(),
                dst: workspace_output_link.clone(),
                err,
            }),
        }
    }

    #[cfg(not(target_os = "windows"))]
    #[tracing::instrument(name = "WorkspacePaths::setup_links")]
    fn setup_links(
        local_outputs_root: &PathBuf,
        workspace_output_link: &PathBuf,
    ) -> Result<(), WorkspacePathsError> {
        let _ = std::fs::remove_file(workspace_output_link);
        match std::os::unix::fs::symlink(local_outputs_root.clone(), workspace_output_link) {
            Ok(_) => Ok(()),
            Err(err) if err.kind() == std::io::ErrorKind::AlreadyExists => Ok(()),
            Err(err) => Err(WorkspacePathsError::CannotCreateSymlink {
                src: local_outputs_root.clone(),
                dst: workspace_output_link.clone(),
                err,
            }),
        }
    }
}
