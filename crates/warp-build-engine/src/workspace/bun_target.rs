use super::label::Label;
use super::short_hash;
use crate::error::{Result, WarpError};
use crate::graph::TaskId;
use serde_json::Value;
use std::path::{Path, PathBuf};
use std::sync::{Arc, OnceLock};

#[derive(Clone)]
pub(super) struct BunBuildTarget {
    pub(super) label: Label,
    pub(super) id: TaskId,
    pub(super) package_name: String,
    pub(super) package_version: String,
    pub(super) package_manifest: Value,
    pub(super) package_root: PathBuf,
    pub(super) package_relative: PathBuf,
    pub(super) source_root: PathBuf,
    pub(super) source_fingerprint: Arc<OnceLock<std::result::Result<String, String>>>,
    pub(super) lockfile: String,
    pub(super) cloudflare_config: String,
}

impl BunBuildTarget {
    pub(super) fn from_directory(
        package_root: &Path,
        workspace_root: &Path,
        work_root: &Path,
        manifest_file: &str,
        lockfiles: &[String],
        cloudflare_config_file: &str,
    ) -> Result<Self> {
        let workspace_root = workspace_root
            .canonicalize()
            .map_err(|error| WarpError::io("resolve workspace root for Bun package", error))?;
        let package_root = package_root
            .canonicalize()
            .map_err(|error| WarpError::io("resolve Bun package directory", error))?;
        let manifest_path = package_root.join(manifest_file);
        let manifest_source = std::fs::read_to_string(&manifest_path)
            .map_err(|error| WarpError::io("read Bun package manifest", error))?;
        let package_manifest: Value = serde_json::from_str(&manifest_source).map_err(|error| {
            WarpError::new(format!("parse {}: {error}", manifest_path.display()))
        })?;
        let package_name = package_manifest
            .get("name")
            .and_then(Value::as_str)
            .map(str::trim)
            .filter(|name| !name.is_empty())
            .ok_or_else(|| {
                WarpError::new(format!(
                    "{} must declare a non-empty package name",
                    manifest_path.display()
                ))
            })?;
        let package_version = package_manifest
            .get("version")
            .and_then(Value::as_str)
            .unwrap_or_default()
            .to_string();

        let package_relative = package_root
            .strip_prefix(&workspace_root)
            .map_err(|_| WarpError::new("Bun package is outside the workspace root"))?
            .to_path_buf();
        let relative_label = package_relative.to_string_lossy().replace('\\', "/");
        let id = TaskId::new(format!("bun:{}", short_hash(&relative_label)))
            .map_err(|error| WarpError::new(error.to_string()))?;
        let label = Label::parse(format!("{package_name}::build"))?;
        let lockfile = lockfiles
            .iter()
            .find(|filename| package_root.join(filename).is_file())
            .cloned()
            .unwrap_or_default();
        let cloudflare_config = if package_root.join(cloudflare_config_file).is_file() {
            cloudflare_config_file.to_string()
        } else {
            String::new()
        };

        Ok(Self {
            label,
            id,
            package_name: package_name.to_string(),
            package_version,
            package_manifest,
            package_root: package_root.clone(),
            package_relative,
            source_root: work_root.join("sources").join(short_hash(&relative_label)),
            source_fingerprint: Arc::new(OnceLock::new()),
            lockfile,
            cloudflare_config,
        })
    }

    pub(super) fn for_path_label(
        label: &Label,
        workspace_root: &Path,
        work_root: &Path,
        cargo_manifest_file: &str,
        bun_manifest_file: &str,
        lockfiles: &[String],
        cloudflare_config_file: &str,
    ) -> Result<Option<Self>> {
        let value = label.as_str();
        let path = Path::new(value);
        let looks_like_path = path.is_absolute()
            || value.starts_with('.')
            || value.contains('/')
            || value.contains('\\');
        if !looks_like_path {
            return Ok(None);
        }
        let requested_path = if path.is_absolute() {
            path.to_path_buf()
        } else {
            workspace_root.join(path)
        };
        let package_root = requested_path
            .canonicalize()
            .map_err(|error| WarpError::io("resolve build-target label", error))?;
        if !package_root.join(bun_manifest_file).is_file() {
            return Ok(None);
        }
        if package_root.join(cargo_manifest_file).is_file() {
            return Ok(None);
        }
        Self::from_directory(
            &package_root,
            workspace_root,
            work_root,
            bun_manifest_file,
            lockfiles,
            cloudflare_config_file,
        )
        .map(Some)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicU64, Ordering};

    static NEXT_TEST: AtomicU64 = AtomicU64::new(0);

    #[test]
    fn path_labels_parse_bun_package_metadata_for_rules() {
        let root = std::env::temp_dir().join(format!(
            "warp-bun-target-test-{}-{}",
            std::process::id(),
            NEXT_TEST.fetch_add(1, Ordering::Relaxed)
        ));
        let package = root.join("sites/example");
        let work = root.join("work");
        std::fs::create_dir_all(&package).unwrap();
        std::fs::write(
            package.join("package.json"),
            r#"{"name":"example-site","version":"1.2.3","packageManager":"bun@1.4.2","scripts":{"build":"vite build"}}"#,
        )
        .unwrap();
        std::fs::write(package.join("bun.lock"), "{}").unwrap();

        let label = Label::parse("./sites/example").unwrap();
        let target = BunBuildTarget::for_path_label(
            &label,
            &root,
            &work,
            "Cargo.toml",
            "package.json",
            &["bun.lock".into(), "bun.lockb".into()],
            "cloudflare.config.ts",
        )
        .unwrap()
        .unwrap();
        assert_eq!(target.label.as_str(), "example-site::build");
        assert_eq!(target.package_version, "1.2.3");
        assert_eq!(target.package_manifest["scripts"]["build"], "vite build");
        assert_eq!(target.lockfile, "bun.lock");
        assert!(target.package_relative.ends_with("sites/example"));

        std::fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn cloudflare_configuration_is_passed_to_the_rule() {
        let root = std::env::temp_dir().join(format!(
            "warp-bun-cf-target-test-{}-{}",
            std::process::id(),
            NEXT_TEST.fetch_add(1, Ordering::Relaxed)
        ));
        std::fs::create_dir_all(&root).unwrap();
        std::fs::write(
            root.join("package.json"),
            r#"{"name":"cf-site","scripts":{"build":"vite build"}}"#,
        )
        .unwrap();
        std::fs::write(root.join("bun.lock"), "{}").unwrap();
        std::fs::write(root.join("cloudflare.config.ts"), "export default {};\n").unwrap();

        let target = BunBuildTarget::from_directory(
            &root,
            &root,
            &root.join("work"),
            "package.json",
            &["bun.lock".into(), "bun.lockb".into()],
            "cloudflare.config.ts",
        )
        .unwrap();
        assert_eq!(target.cloudflare_config, "cloudflare.config.ts");
        std::fs::remove_dir_all(root).unwrap();
    }
}
