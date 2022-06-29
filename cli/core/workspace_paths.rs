use anyhow::{anyhow, Context};
use crypto::digest::Digest;
use crypto::sha1::Sha1;
use directories::ProjectDirs;
use std::path::PathBuf;

#[derive(Clone, Default, Debug)]
pub struct WorkspacePaths {
    /// The user currently using this workspace
    pub current_user: String,

    /// The name of the current workspace
    pub workspace_name: String,

    /// The directories for Warp according to XDG conventions
    // warp_dirs: ProjectDirs,

    /// The location of the global archive
    pub global_archive_root: PathBuf,

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
    pub fn new(
        workspace_root: &PathBuf,
        home: Option<String>,
        user: Option<String>,
    ) -> Result<WorkspacePaths, anyhow::Error> {
        let current_user = user.unwrap_or_else(|| whoami::username());
        let workspace_name = {
            let mut hasher = Sha1::new();
            hasher.input_str(workspace_root.to_str().unwrap());
            format!(
                "{}-{}",
                workspace_root.file_name().unwrap().to_str().unwrap(),
                hasher.result_str()
            )
        };

        let project_dirs = ProjectDirs::from("dev", "abstractmachines", "warp")
            .context("Could not figure out Warp project directories")?;

        let config_dir = if let Some(ref root) = home {
            PathBuf::from(root)
        } else {
            project_dirs.config_dir().to_path_buf()
        };

        let global_cache_dir = if let Some(ref root) = home {
            PathBuf::from(root).join("cache")
        } else {
            project_dirs.cache_dir().to_path_buf()
        };

        let global_rules_root = config_dir.join("rules");
        let global_toolchains_root = config_dir.join("toolchains");

        let user_root = global_cache_dir.join(format!("_user_{}", current_user));

        let global_cache_root = user_root.join("cache").join("global");
        let global_archive_root = user_root.join("archive");
        let global_sandbox_root = user_root.join("sandbox").join("global");

        let local_cache_root = user_root.join("cache").join(&workspace_name);
        let local_sandbox_root = user_root.join("sandbox").join(&workspace_name);
        let local_outputs_root = user_root.join("outputs").join(&workspace_name);

        let workspace_output_link = workspace_root.join("warp-outputs");
        let local_warp_root = workspace_root.join(".warp");
        let local_rules_root = local_warp_root.join("rules");
        let local_toolchains_root = local_warp_root.join("toolchains");

        std::fs::create_dir_all(&global_rules_root)?;
        std::fs::create_dir_all(&global_toolchains_root)?;
        std::fs::create_dir_all(&global_cache_root)?;
        std::fs::create_dir_all(&global_archive_root)?;
        std::fs::create_dir_all(&local_warp_root)?;
        std::fs::create_dir_all(&local_rules_root)?;
        std::fs::create_dir_all(&local_toolchains_root)?;
        std::fs::create_dir_all(&local_sandbox_root)?;
        std::fs::create_dir_all(&local_outputs_root)?;
        WorkspacePaths::setup_links(&local_outputs_root, &workspace_output_link)?;

        let paths = WorkspacePaths {
            current_user,
            workspace_name,
            global_archive_root,
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
    fn setup_links(
        local_outputs_root: &PathBuf,
        workspace_output_link: &PathBuf,
    ) -> Result<(), anyhow::Error> {
        let _ = std::fs::remove_file(workspace_output_link);
        match std::os::windows::fs::symlink_dir(local_outputs_root.clone(), workspace_output_link) {
            Ok(_) => Ok(()),
            Err(err) if err.kind() == std::io::ErrorKind::AlreadyExists => Ok(()),
            err => Err(anyhow!("Could not create symlink because of: {:?}", err)),
        }
    }

    #[cfg(not(target_os = "windows"))]
    fn setup_links(
        local_outputs_root: &PathBuf,
        workspace_output_link: &PathBuf,
    ) -> Result<(), anyhow::Error> {
        let _ = std::fs::remove_file(workspace_output_link);
        match std::os::unix::fs::symlink(local_outputs_root.clone(), workspace_output_link) {
            Ok(_) => Ok(()),
            Err(err) if err.kind() == std::io::ErrorKind::AlreadyExists => Ok(()),
            err => Err(anyhow!("Could not create symlink because of: {:?}", err)),
        }
    }
}
