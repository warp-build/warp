use super::GENERATED_OUTPUT_DIRECTORY;
use crate::error::{Result, WarpError};
use std::fs;
use std::path::{Path, PathBuf};

pub(super) fn discover_directory(
    path: &Path,
    excluded_directories: &[String],
) -> Result<(bool, Vec<PathBuf>)> {
    let canonical = path
        .canonicalize()
        .map_err(|error| WarpError::io("resolve workspace scan directory", error))?;
    let mut children = Vec::new();
    for entry in fs::read_dir(&canonical)
        .map_err(|error| WarpError::io("scan workspace directory", error))?
    {
        let entry =
            entry.map_err(|error| WarpError::io("read workspace directory entry", error))?;
        let file_type = entry
            .file_type()
            .map_err(|error| WarpError::io("inspect workspace directory entry", error))?;
        if !file_type.is_dir() {
            continue;
        }

        let child = entry.path();
        if should_skip(&child, excluded_directories) {
            continue;
        }
        children.push(child);
    }
    children.sort();
    Ok((canonical.join("Cargo.toml").is_file(), children))
}

fn should_skip(path: &Path, excluded_directories: &[String]) -> bool {
    path.file_name()
        .and_then(|name| name.to_str())
        .is_some_and(|name| {
            name == GENERATED_OUTPUT_DIRECTORY
                || excluded_directories.iter().any(|excluded| excluded == name)
        })
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicU64, Ordering};

    static NEXT_TEST: AtomicU64 = AtomicU64::new(0);

    #[test]
    fn discovers_manifests_and_applies_client_exclusions() {
        let id = NEXT_TEST.fetch_add(1, Ordering::Relaxed);
        let root =
            std::env::temp_dir().join(format!("warp-workspace-scan-{}-{id}", std::process::id()));
        fs::create_dir_all(root.join("crates/example")).unwrap();
        fs::create_dir_all(root.join("_build/generated")).unwrap();
        fs::create_dir_all(root.join("excluded-cache/generated")).unwrap();
        fs::create_dir_all(root.join("excluded-sources/old")).unwrap();
        fs::write(root.join("Cargo.toml"), "[workspace]").unwrap();
        fs::write(root.join("crates/example/Cargo.toml"), "[package]").unwrap();
        fs::write(root.join("_build/generated/Cargo.toml"), "[package]").unwrap();
        fs::write(
            root.join("excluded-cache/generated/Cargo.toml"),
            "[package]",
        )
        .unwrap();
        fs::write(root.join("excluded-sources/old/Cargo.toml"), "[package]").unwrap();

        let excludes = vec!["excluded-cache".to_string(), "excluded-sources".to_string()];
        let (has_manifest, children) = discover_directory(&root, &excludes).unwrap();

        assert!(has_manifest);
        assert!(children.iter().any(|path| path.ends_with("crates")));
        assert!(!children.iter().any(|path| path.ends_with("_build")));
        assert!(!children.iter().any(|path| path.ends_with("excluded-cache")));
        assert!(!children
            .iter()
            .any(|path| path.ends_with("excluded-sources")));
        fs::remove_dir_all(root).unwrap();
    }
}
