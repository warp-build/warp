use super::*;
use anyhow::Context;
use log::*;
use std::fs;
use std::path::PathBuf;

/// A struct to scan Workspace paths to find different files.
pub struct WorkspaceScanner {
    /// The paths used to scan.
    paths: WorkspacePaths,
}

impl WorkspaceScanner {
    pub fn find_workspace_file(cwd: &PathBuf) -> Result<(PathBuf, PathBuf), anyhow::Error> {
        let here = &cwd.join(WORKSPACE);
        debug!("Searching for workspace file in {:?}", here);
        if fs::metadata(here).is_ok() {
            debug!("Found it! Continuing...");
            let root = std::fs::canonicalize(here.parent().unwrap())?;
            Ok((root.to_path_buf(), here.to_path_buf()))
        } else {
            let parent = cwd.parent().context(
                "Reached the top of the file system and could not find a workspace file",
            )?;
            WorkspaceScanner::find_workspace_file(&parent.to_path_buf())
        }
    }

    pub fn from_paths(paths: &WorkspacePaths) -> WorkspaceScanner {
        WorkspaceScanner {
            paths: paths.clone(),
        }
    }

    pub fn find_build_files(&self) -> Result<Vec<PathBuf>, anyhow::Error> {
        debug!(
            "Scanning for build files in {:?}",
            self.paths.workspace_root
        );
        let paths = FileScanner::new()
            .matching_path(ZAPFILE)?
            .starting_from(&self.paths.workspace_root)
            .find_files()?;

        debug!("Found {} build files...", paths.len());
        for p in &paths {
            debug!("* {:?}", p.to_str().unwrap())
        }

        Ok(paths)
    }

    pub fn find_rules(&self) -> Result<Vec<PathBuf>, anyhow::Error> {
        debug!(
            "Scanning for local rules in {:?}",
            self.paths.local_rules_root
        );
        let paths = FileScanner::new()
            .starting_from(&self.paths.local_rules_root)
            .matching_path("\\.js$")?
            .find_files()?;

        debug!("Found {} local rules...", paths.len());
        for p in &paths {
            debug!("* {}", p.to_str().unwrap())
        }

        Ok(paths)
    }

    pub fn find_toolchains(&self) -> Result<Vec<PathBuf>, anyhow::Error> {
        debug!(
            "Scanning for local toolchains in {:?}",
            self.paths.local_toolchains_root
        );

        let paths = FileScanner::new()
            .starting_from(&self.paths.local_toolchains_root)
            .matching_path("\\.js$")?
            .find_files()?;

        debug!("Found {} local toolchains...", paths.len());
        for p in &paths {
            debug!("* {}", p.to_str().unwrap())
        }

        Ok(paths)
    }
}
