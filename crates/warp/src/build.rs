use crate::cache;
use crate::cli::{crate_name, BuildRequest};
use crate::error::{Result, WarpError};
use crate::fingerprint::{action_key, rustc, SourceTree};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::atomic::{AtomicU64, Ordering};

static NEXT_WORK: AtomicU64 = AtomicU64::new(0);

#[derive(Debug, PartialEq, Eq)]
pub enum BuildOutcome {
    Compiled { key: String },
    CacheHit { key: String },
}

pub fn build(request: &BuildRequest) -> Result<BuildOutcome> {
    let entry = request
        .entry
        .canonicalize()
        .map_err(|error| WarpError::io("resolve Rust entry point", error))?;
    let source_root = entry
        .parent()
        .ok_or_else(|| WarpError::new("Rust entry point has no source directory"))?;

    fs::create_dir_all(&request.cache)
        .map_err(|error| WarpError::io("create build cache directory", error))?;
    let cache_root = request
        .cache
        .canonicalize()
        .map_err(|error| WarpError::io("resolve build cache directory", error))?;
    if cache_root.starts_with(source_root) {
        return Err(WarpError::new(
            "build cache must be outside the Rust source directory",
        ));
    }

    let output_parent = request
        .output
        .parent()
        .filter(|parent| !parent.as_os_str().is_empty())
        .unwrap_or_else(|| Path::new("."));
    fs::create_dir_all(output_parent)
        .map_err(|error| WarpError::io("create output directory", error))?;
    let output_parent = output_parent
        .canonicalize()
        .map_err(|error| WarpError::io("resolve output directory", error))?;
    if output_parent.starts_with(source_root) || output_parent.starts_with(&cache_root) {
        return Err(WarpError::new(
            "build output must be outside source and cache directories",
        ));
    }

    let output_name = request
        .output
        .file_name()
        .ok_or_else(|| WarpError::new("output path must name a file"))?;
    let output = output_parent.join(output_name);
    let work = WorkDirectory::create(&cache_root)?;
    let source_tree = SourceTree::capture(request, &work.path.join("source"))?;
    let (compiler, compiler_version) = rustc()?;
    let key = action_key(request, &source_tree, &compiler, &compiler_version)?;

    if cache::restore(&cache_root, &key, &output)? {
        return Ok(BuildOutcome::CacheHit { key });
    }

    let compiled_artifact = compile(request, &source_tree, &work.path.join("output"), &compiler)?;
    cache::store(&cache_root, &key, &compiled_artifact)?;
    cache::copy_atomically(&compiled_artifact, &output)?;

    Ok(BuildOutcome::Compiled { key })
}

fn compile(
    request: &BuildRequest,
    source_tree: &SourceTree,
    output_directory: &Path,
    compiler: &Path,
) -> Result<PathBuf> {
    fs::create_dir_all(output_directory)
        .map_err(|error| WarpError::io("create rustc output directory", error))?;
    let name = crate_name(&request.entry)?;
    let mut command = Command::new(compiler);
    command
        .current_dir(&source_tree.root)
        .arg("--crate-name")
        .arg(&name)
        .arg("--crate-type")
        .arg("bin")
        .arg("--edition")
        .arg(request.edition.as_str())
        .arg("--emit=link")
        .arg("--out-dir")
        .arg(output_directory)
        .arg("--remap-path-prefix")
        .arg(format!("{}=/warp-source", source_tree.root.display()))
        .arg("--remap-path-prefix")
        .arg(format!("{}=/warp-output", output_directory.display()))
        .arg(&source_tree.entry);

    let output = command
        .output()
        .map_err(|error| WarpError::io("run rustc", error))?;
    if !output.status.success() {
        return Err(WarpError::new(format!(
            "rustc failed with {}\n{}{}",
            output.status,
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        )));
    }

    let executable = executable_name(&name);
    let artifact = output_directory.join(executable);
    if !artifact.is_file() {
        return Err(WarpError::new(format!(
            "rustc succeeded but did not produce expected executable {}",
            artifact.display()
        )));
    }
    Ok(artifact)
}

fn executable_name(crate_name: &str) -> String {
    if cfg!(windows) {
        format!("{crate_name}.exe")
    } else {
        crate_name.to_string()
    }
}

struct WorkDirectory {
    path: PathBuf,
}

impl WorkDirectory {
    fn create(cache: &Path) -> Result<Self> {
        loop {
            let id = NEXT_WORK.fetch_add(1, Ordering::Relaxed);
            let path = cache.join(format!(".warp-work-{}-{id}", std::process::id()));
            match fs::create_dir(&path) {
                Ok(()) => return Ok(Self { path }),
                Err(error) if error.kind() == std::io::ErrorKind::AlreadyExists => continue,
                Err(error) => return Err(WarpError::io("create build work directory", error)),
            }
        }
    }
}

impl Drop for WorkDirectory {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.path);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cli::Edition;
    use std::fs;
    use std::sync::atomic::{AtomicU64, Ordering};

    static NEXT_TEST: AtomicU64 = AtomicU64::new(0);

    struct TestDirectory(PathBuf);

    impl TestDirectory {
        fn new() -> Self {
            let id = NEXT_TEST.fetch_add(1, Ordering::Relaxed);
            let path =
                std::env::temp_dir().join(format!("warp-build-test-{}-{id}", std::process::id()));
            fs::create_dir_all(&path).unwrap();
            Self(path)
        }
    }

    impl Drop for TestDirectory {
        fn drop(&mut self) {
            let _ = fs::remove_dir_all(&self.0);
        }
    }

    fn request(directory: &Path) -> BuildRequest {
        BuildRequest {
            entry: directory.join("src/main.rs"),
            output: directory.join("out/hello"),
            cache: directory.join("cache"),
            edition: Edition::E2021,
        }
    }

    #[test]
    fn compiles_rust_and_reuses_only_matching_inputs() {
        let directory = TestDirectory::new();
        fs::create_dir_all(directory.0.join("src")).unwrap();
        fs::write(
            directory.0.join("src/main.rs"),
            "mod greeting; fn main() { print!(\"{}\", greeting::message()); }\n",
        )
        .unwrap();
        fs::write(
            directory.0.join("src/greeting.rs"),
            "pub fn message() -> &'static str { \"hello\" }\n",
        )
        .unwrap();
        let request = request(&directory.0);

        assert!(matches!(
            build(&request).unwrap(),
            BuildOutcome::Compiled { .. }
        ));
        assert_eq!(run_program(&request.output), "hello");
        assert!(matches!(
            build(&request).unwrap(),
            BuildOutcome::CacheHit { .. }
        ));

        fs::write(
            directory.0.join("src/greeting.rs"),
            "pub fn message() -> &'static str { \"warp\" }\n",
        )
        .unwrap();
        assert!(matches!(
            build(&request).unwrap(),
            BuildOutcome::Compiled { .. }
        ));
        assert_eq!(run_program(&request.output), "warp");
    }

    #[test]
    fn rejects_source_or_cache_output_overlap() {
        let directory = TestDirectory::new();
        fs::create_dir_all(directory.0.join("src")).unwrap();
        fs::write(directory.0.join("src/main.rs"), "fn main() {}\n").unwrap();
        let mut request = request(&directory.0);
        request.output = directory.0.join("src/overwritten");
        assert!(build(&request)
            .unwrap_err()
            .to_string()
            .contains("outside source"));
    }

    fn run_program(path: &Path) -> String {
        let output = Command::new(path).output().unwrap();
        assert!(output.status.success());
        String::from_utf8(output.stdout).unwrap()
    }
}
