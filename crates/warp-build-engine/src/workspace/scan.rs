use super::GENERATED_OUTPUT_DIRECTORY;
use crate::error::{Result, WarpError};
use ignore::WalkBuilder;
use std::path::{Path, PathBuf};

pub(super) fn discover_directory(
    path: &Path,
    excluded_directories: &[String],
    cargo_manifest_file: &str,
    bun_manifest_file: &str,
) -> Result<(bool, bool, Vec<PathBuf>)> {
    let canonical = path
        .canonicalize()
        .map_err(|error| WarpError::io("resolve workspace scan directory", error))?;
    let mut walker = WalkBuilder::new(&canonical);
    let excluded_directories = excluded_directories.to_vec();
    walker
        .max_depth(Some(1))
        .hidden(false)
        .parents(true)
        .ignore(false)
        .git_ignore(true)
        .git_global(false)
        .git_exclude(false)
        .follow_links(false)
        .filter_entry(move |entry| {
            entry.depth() == 0 || !should_skip_directory(entry.path(), &excluded_directories)
        });

    let mut has_cargo_manifest = false;
    let mut has_bun_manifest = false;
    let mut children = Vec::new();
    for result in walker.build() {
        let entry =
            result.map_err(|error| WarpError::new(format!("walk workspace directory: {error}")))?;
        if entry.depth() != 1 {
            continue;
        }
        let is_file = entry.file_type().is_some_and(|kind| kind.is_file());
        let is_directory = entry.file_type().is_some_and(|kind| kind.is_dir());
        let is_cargo_manifest = entry.file_name() == cargo_manifest_file && is_file;
        let is_bun_manifest = entry.file_name() == bun_manifest_file && is_file;
        let path = entry.into_path();
        has_cargo_manifest |= is_cargo_manifest;
        has_bun_manifest |= is_bun_manifest;
        if is_directory {
            children.push(path);
        }
    }
    children.sort();
    Ok((has_cargo_manifest, has_bun_manifest, children))
}

pub(super) fn should_skip_directory(path: &Path, excluded_directories: &[String]) -> bool {
    path.file_name()
        .and_then(|name| name.to_str())
        .is_some_and(|name| {
            name == GENERATED_OUTPUT_DIRECTORY
                || name == ".git"
                || excluded_directories.iter().any(|excluded| excluded == name)
        })
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use std::sync::atomic::{AtomicU64, Ordering};

    static NEXT_TEST: AtomicU64 = AtomicU64::new(0);

    #[test]
    fn discovers_manifests_and_applies_client_exclusions() {
        let id = NEXT_TEST.fetch_add(1, Ordering::Relaxed);
        let root =
            std::env::temp_dir().join(format!("warp-workspace-scan-{}-{id}", std::process::id()));
        fs::create_dir_all(root.join("crates/example")).unwrap();
        fs::create_dir_all(root.join("_build/generated")).unwrap();
        fs::create_dir_all(root.join(".git/objects")).unwrap();
        fs::create_dir_all(root.join("excluded-cache/generated")).unwrap();
        fs::create_dir_all(root.join("excluded-sources/old")).unwrap();
        fs::write(root.join("Cargo.toml"), "[workspace]").unwrap();
        fs::write(root.join("package.json"), "{}").unwrap();
        fs::write(root.join("crates/example/Cargo.toml"), "[package]").unwrap();
        fs::write(root.join("_build/generated/Cargo.toml"), "[package]").unwrap();
        fs::write(root.join(".git/objects/Cargo.toml"), "[package]").unwrap();
        fs::write(
            root.join("excluded-cache/generated/Cargo.toml"),
            "[package]",
        )
        .unwrap();
        fs::write(root.join("excluded-sources/old/Cargo.toml"), "[package]").unwrap();
        fs::write(
            root.join(".gitignore"),
            "excluded-cache/\n!excluded-cache/generated/\n_build/\n!_build/\n!_build/generated/\n",
        )
        .unwrap();

        let excludes = vec!["excluded-sources".to_string()];
        let (has_cargo_manifest, has_bun_manifest, children) =
            discover_directory(&root, &excludes, "Cargo.toml", "package.json").unwrap();

        assert!(has_cargo_manifest);
        assert!(has_bun_manifest);
        assert!(children.iter().any(|path| path.ends_with("crates")));
        assert!(!children.iter().any(|path| path.ends_with("_build")));
        assert!(!children.iter().any(|path| path.ends_with(".git")));
        assert!(!children.iter().any(|path| path.ends_with("excluded-cache")));
        assert!(!children
            .iter()
            .any(|path| path.ends_with("excluded-sources")));
        fs::remove_dir_all(root).unwrap();
    }
}
