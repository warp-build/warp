use crate::error::{Result, WarpError};
use crate::{crate_name, BuildRequest, CompilationTargetTriple, RustToolchainChannel};
use std::ffi::OsString;
use std::fs::{self, File};
use std::io::Read;
use std::path::{Path, PathBuf};
use std::process::Command;

pub struct SourceTree {
    pub root: PathBuf,
    pub entry: PathBuf,
}

impl SourceTree {
    pub fn capture(request: &BuildRequest, destination: &Path) -> Result<Self> {
        let entry = request
            .entry
            .canonicalize()
            .map_err(|error| WarpError::io("resolve Rust entry point", error))?;
        if !entry.is_file() || entry.extension().and_then(|ext| ext.to_str()) != Some("rs") {
            return Err(WarpError::new(format!(
                "Rust entry point must be an existing .rs file: {}",
                request.entry.display()
            )));
        }

        let root = entry
            .parent()
            .ok_or_else(|| WarpError::new("Rust entry point has no parent directory"))?;
        let files = collect_files(root)?;
        for relative in files {
            let source = root.join(&relative);
            let snapshot = destination.join(&relative);
            let parent = snapshot
                .parent()
                .ok_or_else(|| WarpError::new("source snapshot path has no parent"))?;
            fs::create_dir_all(parent)
                .map_err(|error| WarpError::io("create source snapshot directory", error))?;
            fs::copy(&source, &snapshot)
                .map_err(|error| WarpError::io("copy source into immutable snapshot", error))?;
        }

        let relative_entry = entry
            .strip_prefix(root)
            .map_err(|_| WarpError::new("entry point is outside its source tree"))?;
        Ok(Self {
            root: destination.to_path_buf(),
            entry: destination.join(relative_entry),
        })
    }
}

pub fn action_key(
    request: &BuildRequest,
    source_tree: &SourceTree,
    compiler: &Path,
    compiler_version: &str,
) -> Result<String> {
    let crate_name = crate_name(&request.entry)?;
    let files = collect_files(&source_tree.root)?;
    let mut environment: Vec<(OsString, OsString)> = std::env::vars_os().collect();
    environment.sort();

    let mut hasher = blake3::Hasher::new();
    {
        write_field(&mut hasher, b"warp-rust-action-v1")?;
        write_field(&mut hasher, compiler.to_string_lossy().as_bytes())?;
        write_field(&mut hasher, compiler_version.as_bytes())?;
        write_field(&mut hasher, request.edition.as_str().as_bytes())?;
        write_field(&mut hasher, request.compilation_target.as_str().as_bytes())?;
        write_field(&mut hasher, request.profile.as_str().as_bytes())?;
        write_field(&mut hasher, crate_name.as_bytes())?;
        write_field(&mut hasher, std::env::consts::OS.as_bytes())?;
        write_field(&mut hasher, std::env::consts::ARCH.as_bytes())?;
        write_field(
            &mut hasher,
            source_tree
                .entry
                .strip_prefix(&source_tree.root)
                .map_err(|_| WarpError::new("entry point is outside its source snapshot"))?
                .to_string_lossy()
                .as_bytes(),
        )?;

        for (name, value) in environment {
            write_field(&mut hasher, name.to_string_lossy().as_bytes())?;
            write_field(&mut hasher, value.to_string_lossy().as_bytes())?;
        }

        for relative in files {
            let relative_name = relative
                .to_str()
                .ok_or_else(|| WarpError::new("source paths must be valid UTF-8"))?
                .replace('\\', "/");
            write_field(&mut hasher, relative_name.as_bytes())?;
            let path = source_tree.root.join(relative);
            let mut file = File::open(&path)
                .map_err(|error| WarpError::io("read source snapshot file", error))?;
            let length = file
                .metadata()
                .map_err(|error| WarpError::io("inspect source snapshot file", error))?
                .len()
                .to_le_bytes();
            write_field(&mut hasher, &length)?;
            let mut buffer = [0; 32 * 1024];
            loop {
                let count = file
                    .read(&mut buffer)
                    .map_err(|error| WarpError::io("read source snapshot file", error))?;
                if count == 0 {
                    break;
                }
                hasher.update(&buffer[..count]);
            }
        }
    }
    Ok(format!("blake3:{}", hasher.finalize().to_hex()))
}

pub fn host_triple() -> Result<CompilationTargetTriple> {
    let host = guess_host_triple::guess_host_triple()
        .ok_or_else(|| WarpError::new("could not detect the host target triple"))?;
    CompilationTargetTriple::parse(host)
}

pub fn rustc() -> Result<(PathBuf, String)> {
    rustc_for_toolchain(None)
}

pub fn rustc_for_toolchain(toolchain: Option<&RustToolchainChannel>) -> Result<(PathBuf, String)> {
    let requested = match toolchain {
        Some(toolchain) => {
            let output = Command::new("rustup")
                .args(["which", "rustc", "--toolchain", toolchain.as_str()])
                .output()
                .map_err(|error| WarpError::io("resolve configured Rust toolchain", error))?;
            if !output.status.success() {
                return Err(WarpError::new(format!(
                    "rustup could not resolve Rust toolchain {toolchain}: {}",
                    String::from_utf8_lossy(&output.stderr).trim()
                )));
            }
            OsString::from(
                String::from_utf8(output.stdout)
                    .map_err(|error| {
                        WarpError::new(format!("rustup returned invalid UTF-8: {error}"))
                    })?
                    .trim(),
            )
        }
        None => std::env::var_os("RUSTC").unwrap_or_else(|| OsString::from("rustc")),
    };
    let path = find_executable(&requested)?;
    let output = Command::new(&path)
        .arg("-vV")
        .output()
        .map_err(|error| WarpError::io("query rustc version", error))?;
    if !output.status.success() {
        return Err(WarpError::new(format!(
            "rustc version query failed: {}",
            String::from_utf8_lossy(&output.stderr).trim()
        )));
    }
    let version = String::from_utf8(output.stdout)
        .map_err(|error| WarpError::new(format!("rustc returned invalid UTF-8: {error}")))?;
    Ok((path, version))
}

fn find_executable(requested: &std::ffi::OsStr) -> Result<PathBuf> {
    let requested_path = Path::new(requested);
    if requested_path.components().count() > 1 {
        return requested_path
            .canonicalize()
            .map_err(|error| WarpError::io("resolve rustc executable", error));
    }

    let search_path = std::env::var_os("PATH")
        .ok_or_else(|| WarpError::new("PATH is not set; cannot find rustc"))?;
    for directory in std::env::split_paths(&search_path) {
        let candidate = directory.join(requested_path);
        if candidate.is_file() {
            return candidate
                .canonicalize()
                .map_err(|error| WarpError::io("resolve rustc executable", error));
        }
    }
    Err(WarpError::new(format!(
        "could not find rustc executable {:?} on PATH",
        requested
    )))
}

fn collect_files(root: &Path) -> Result<Vec<PathBuf>> {
    fn visit(root: &Path, directory: &Path, files: &mut Vec<PathBuf>) -> Result<()> {
        let mut entries = fs::read_dir(directory)
            .map_err(|error| WarpError::io("read Rust source directory", error))?
            .map(|entry| entry.map_err(|error| WarpError::io("read Rust source entry", error)))
            .collect::<Result<Vec<_>>>()?;
        entries.sort_by_key(|entry| entry.file_name());

        for entry in entries {
            let file_type = entry
                .file_type()
                .map_err(|error| WarpError::io("inspect Rust source entry", error))?;
            let path = entry.path();
            if file_type.is_symlink() {
                return Err(WarpError::new(format!(
                    "symbolic links are not supported in a Rust source tree: {}",
                    path.display()
                )));
            }
            if file_type.is_dir() {
                visit(root, &path, files)?;
            } else if file_type.is_file() {
                files.push(
                    path.strip_prefix(root)
                        .map_err(|_| WarpError::new("source file escaped its source tree"))?
                        .to_path_buf(),
                );
            }
        }
        Ok(())
    }

    let mut files = Vec::new();
    visit(root, root, &mut files)?;
    files.sort();
    if files.is_empty() {
        return Err(WarpError::new("Rust source tree contains no files"));
    }
    Ok(files)
}

fn write_field(hasher: &mut blake3::Hasher, bytes: &[u8]) -> Result<()> {
    let length =
        u64::try_from(bytes.len()).map_err(|_| WarpError::new("fingerprint field is too large"))?;
    hasher.update(&length.to_le_bytes());
    hasher.update(bytes);
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{host_triple, BuildProfile, CompilationTargetTriple, Edition};
    use std::sync::atomic::{AtomicU64, Ordering};

    static NEXT_TEMP: AtomicU64 = AtomicU64::new(0);

    struct TestDirectory(PathBuf);

    impl TestDirectory {
        fn new() -> Self {
            let id = NEXT_TEMP.fetch_add(1, Ordering::Relaxed);
            let path = std::env::temp_dir()
                .join(format!("warp-fingerprint-test-{}-{id}", std::process::id()));
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
            output: directory.join("out/warp"),
            cache: Some(directory.join("cache")),
            edition: Edition::E2021,
            compilation_target: host_triple().unwrap(),
            rust_toolchain: None,
            profile: BuildProfile::Debug,
        }
    }

    #[test]
    fn snapshots_include_all_files_and_ignore_original_root_location() {
        let first = TestDirectory::new();
        let second = TestDirectory::new();
        for directory in [&first.0, &second.0] {
            fs::create_dir_all(directory.join("src")).unwrap();
            fs::write(directory.join("src/main.rs"), "fn main() {}\n").unwrap();
            fs::write(directory.join("src/config.txt"), "enabled\n").unwrap();
        }

        let first_request = request(&first.0);
        let second_request = request(&second.0);
        let first_tree = SourceTree::capture(&first_request, &first.0.join("snapshot")).unwrap();
        let second_tree = SourceTree::capture(&second_request, &second.0.join("snapshot")).unwrap();
        let compiler = Path::new("/toolchain/rustc");

        let first_key = action_key(&first_request, &first_tree, compiler, "rustc test").unwrap();
        let second_key = action_key(&second_request, &second_tree, compiler, "rustc test").unwrap();
        assert_eq!(first_key, second_key);

        let mut different_target = first_request.clone();
        different_target.compilation_target =
            CompilationTargetTriple::parse("wasm32-wasip1").unwrap();
        assert_ne!(
            first_key,
            action_key(&different_target, &first_tree, compiler, "rustc test").unwrap()
        );
        let mut different_profile = first_request.clone();
        different_profile.profile = BuildProfile::Release;
        assert_ne!(
            first_key,
            action_key(&different_profile, &first_tree, compiler, "rustc test").unwrap()
        );

        fs::write(second_tree.root.join("config.txt"), "disabled\n").unwrap();
        let changed_key =
            action_key(&second_request, &second_tree, compiler, "rustc test").unwrap();
        assert_ne!(first_key, changed_key);
    }

    #[test]
    fn rejects_symbolic_links_instead_of_omitting_inputs() {
        let directory = TestDirectory::new();
        let source_root = directory.0.join("src");
        fs::create_dir_all(&source_root).unwrap();
        fs::write(source_root.join("main.rs"), "fn main() {}\n").unwrap();
        #[cfg(unix)]
        std::os::unix::fs::symlink("main.rs", source_root.join("linked.rs")).unwrap();

        #[cfg(unix)]
        assert!(collect_files(&source_root).is_err());
    }
}
