use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};
use std::sync::atomic::{AtomicU64, Ordering};

static NEXT_TEST: AtomicU64 = AtomicU64::new(0);

struct TestDirectory(PathBuf);

impl TestDirectory {
    fn new() -> Self {
        let id = NEXT_TEST.fetch_add(1, Ordering::Relaxed);
        let path =
            std::env::temp_dir().join(format!("warp-bootstrap-test-{}-{id}", std::process::id()));
        fs::create_dir_all(&path).unwrap();
        Self(path)
    }
}

impl Drop for TestDirectory {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.0);
    }
}

#[cfg(unix)]
#[test]
fn default_build_discovers_and_builds_rust_and_bun_targets() {
    use std::os::unix::fs::PermissionsExt;

    let directory = TestDirectory::new();
    let project = directory.0.join("project");
    let cache = directory.0.join("shared-cache");
    let rust_package = project.join("crates/hello");
    let site = project.join("sites/example");
    let fake_bin = directory.0.join("bin");
    fs::create_dir_all(rust_package.join("src")).unwrap();
    fs::create_dir_all(&site).unwrap();
    fs::create_dir_all(&fake_bin).unwrap();
    fs::write(
        project.join("Cargo.toml"),
        r#"[workspace]
members = ["crates/hello"]
resolver = "2"
"#,
    )
    .unwrap();
    fs::write(
        rust_package.join("Cargo.toml"),
        r#"[package]
name = "hello"
version = "0.1.0"
edition = "2021"
"#,
    )
    .unwrap();
    fs::write(rust_package.join("src/main.rs"), "fn main() {}\n").unwrap();
    fs::write(
        site.join("package.json"),
        r#"{"name":"example-site","version":"1.0.0","packageManager":"bun@1.4.2","scripts":{"build":"fake build"}}"#,
    )
    .unwrap();
    fs::write(site.join("bun.lock"), "{}\n").unwrap();
    let fake_bun = fake_bin.join("bun");
    fs::write(
        &fake_bun,
        "#!/bin/sh\nif [ \"$1\" = \"--version\" ]; then echo 1.4.2; exit 0; fi\nif [ \"$1\" = \"run\" ] && [ \"$2\" = \"build\" ]; then mkdir -p dist; echo site-built > dist/index.html; fi\nexit 0\n",
    )
    .unwrap();
    fs::set_permissions(&fake_bun, fs::Permissions::from_mode(0o755)).unwrap();
    let lockfile = Command::new("cargo")
        .args(["generate-lockfile", "--manifest-path"])
        .arg(project.join("Cargo.toml"))
        .output()
        .unwrap();
    assert_success(&lockfile);

    let original_path = std::env::var_os("PATH").unwrap_or_default();
    let path = std::iter::once(fake_bin)
        .chain(std::env::split_paths(&original_path))
        .collect::<Vec<_>>();
    let path = std::env::join_paths(path).unwrap();
    let target = warp_build_engine::host_triple().unwrap();
    let build = Command::new(env!("CARGO_BIN_EXE_warp"))
        .current_dir(&project)
        .args(["build", "--target"])
        .arg(target.as_str())
        .arg("--cache")
        .arg(&cache)
        .env("PATH", path)
        .output()
        .unwrap();
    assert_success(&build);
    let output = output_text(&build);
    assert!(output.contains("example-site::build"), "{output}");
    assert!(output.contains("hello::hello"), "{output}");
    assert!(output.contains("FINISHED 2 targets"), "{output}");
    assert!(project
        .join("_build")
        .join(target.as_str())
        .join("debug/sites/example/dist/index.html")
        .is_file());
    assert!(project
        .join("_build")
        .join(target.as_str())
        .join("debug/crates/hello/hello")
        .is_file());
}

#[test]
fn package_path_builds_only_that_crate_and_its_dependencies() {
    let directory = TestDirectory::new();
    let project = directory.0.join("project");
    let cache = directory.0.join("shared-cache");
    let selected = project.join("crates/warp-store");
    let dependency = project.join("crates/common");
    let unselected = project.join("crates/unused");
    fs::create_dir_all(selected.join("src")).unwrap();
    fs::create_dir_all(dependency.join("src")).unwrap();
    fs::create_dir_all(unselected.join("src")).unwrap();
    fs::write(
        project.join("Cargo.toml"),
        r#"[workspace]
members = ["crates/warp-store", "crates/common", "crates/unused"]
resolver = "2"
"#,
    )
    .unwrap();
    for (package, body) in [
        (
            &dependency,
            r#"pub fn common() {}
"#,
        ),
        (
            &selected,
            r#"pub fn selected() { common::common() }
"#,
        ),
        (
            &unselected,
            r#"pub fn unused() {}
"#,
        ),
    ] {
        let name = package.file_name().unwrap().to_string_lossy();
        fs::write(
            package.join("Cargo.toml"),
            format!(
                r#"[package]
name = "{name}"
version = "0.1.0"
edition = "2021"
"#
            ),
        )
        .unwrap();
        fs::write(package.join("src/lib.rs"), body).unwrap();
    }
    fs::write(
        selected.join("Cargo.toml"),
        r#"[package]
name = "warp-store"
version = "0.1.0"
edition = "2021"

[dependencies]
common = { path = "../common" }
"#,
    )
    .unwrap();
    let lockfile = Command::new("cargo")
        .args(["generate-lockfile", "--manifest-path"])
        .arg(project.join("Cargo.toml"))
        .output()
        .unwrap();
    assert_success(&lockfile);

    let target = warp_build_engine::host_triple().unwrap();
    let output = Command::new(env!("CARGO_BIN_EXE_warp"))
        .current_dir(&project)
        .args(["build", "./crates/warp-store", "--target"])
        .arg(target.as_str())
        .arg("--cache")
        .arg(&cache)
        .output()
        .unwrap();
    assert_success(&output);
    assert!(output_text(&output).contains("Building warp-store::warp_store"));
    assert!(output_text(&output).contains("FINISHED 2 targets"));
    assert!(project
        .join("_build")
        .join(target.as_str())
        .join("debug/crates/warp-store/libwarp_store.rlib")
        .is_file());
    assert!(project
        .join("_build")
        .join(target.as_str())
        .join("debug/crates/common/libcommon.rlib")
        .is_file());
    assert!(!project
        .join("_build")
        .join(target.as_str())
        .join("debug/crates/unused")
        .exists());
}

#[test]
fn release_profile_mirrors_workspace_artifacts_and_promotes_the_binary() {
    let directory = TestDirectory::new();
    let project = directory.0.join("project");
    let cache = directory.0.join("shared-cache");
    fs::create_dir_all(project.join("src")).unwrap();
    fs::write(
        project.join("Cargo.toml"),
        r#"[package]
name = "tiny-tool"
version = "0.1.0"
edition = "2021"
"#,
    )
    .unwrap();
    fs::write(
        project.join("src/main.rs"),
        r#"fn main() { println!("workspace-layout-ok"); }
"#,
    )
    .unwrap();
    let lockfile = Command::new("cargo")
        .args(["generate-lockfile", "--manifest-path"])
        .arg(project.join("Cargo.toml"))
        .output()
        .unwrap();
    assert_success(&lockfile);

    let target = warp_build_engine::host_triple().unwrap();
    let warp = env!("CARGO_BIN_EXE_warp");
    let build = |binary: &str| {
        Command::new(binary)
            .current_dir(&project)
            .args(["build", "--profile", "release", "--target"])
            .arg(target.as_str())
            .arg("--cache")
            .arg(&cache)
            .output()
            .unwrap()
    };
    let first = build(warp);
    assert_success(&first);
    assert!(output_text(&first).contains("Building tiny-tool::tiny-tool"));
    assert!(output_text(&first).contains("OK tiny-tool::tiny-tool"));
    assert!(output_text(&first).contains("FINISHED 1 targets"));

    let rustc_name = warp_build_engine::rustc_output_filename(&target, "tiny_tool", "bin").unwrap();
    let target_suffix = rustc_name.strip_prefix("tiny_tool").unwrap();
    let binary_name = format!("tiny-tool{target_suffix}");
    let promoted = project
        .join("_build")
        .join(target.as_str())
        .join("release/bin")
        .join(&binary_name);
    let mirrored = project
        .join("_build")
        .join(target.as_str())
        .join("release")
        .join(&binary_name);
    assert!(promoted.is_file());
    assert!(mirrored.is_file());
    let output = Command::new(&promoted).output().unwrap();
    assert_success(&output);
    assert_eq!(
        String::from_utf8_lossy(&output.stdout).trim(),
        "workspace-layout-ok"
    );

    let second = build(warp);
    assert_success(&second);
    assert!(output_text(&second).contains("OK tiny-tool::tiny-tool "));
    assert!(output_text(&second).contains("s (cached)"));
    assert!(output_text(&second).contains("FINISHED 1 targets"));

    fs::write(project.join("src/main.rs"), "fn main( {\n").unwrap();
    let failed = build(warp);
    assert!(!failed.status.success());
    assert!(
        output_text(&failed).contains("ERR tiny-tool::tiny-tool "),
        "{}",
        output_text(&failed)
    );
    assert!(output_text(&failed).contains("rustc failed"));
    assert!(output_text(&failed).contains("FINISHED build failed"));
}

#[test]
fn cargo_built_warp_builds_warp_and_then_reuses_the_workspace_cache() {
    let directory = TestDirectory::new();
    let output = directory.0.join("debug/warp");
    let cache = directory.0.join("cache");
    let workspace_root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(Path::parent)
        .expect("warp-cli should be two levels below the workspace root");
    let cargo_target_directory = workspace_root.join("_build/cargo/stable");
    let cargo_built_warp = env!("CARGO_BIN_EXE_warp");
    let rustc = Command::new("rustup")
        .args(["which", "rustc", "--toolchain", "stable"])
        .output()
        .unwrap();
    assert_success(&rustc);
    let rustc = String::from_utf8(rustc.stdout).unwrap();
    let stable_cargo_build = Command::new("rustup")
        .args(["run", "stable", "cargo", "build", "--workspace", "--locked"])
        .current_dir(workspace_root)
        .env("RUSTC", rustc.trim())
        .env("CARGO_TARGET_DIR", &cargo_target_directory)
        .output()
        .unwrap();
    assert_success(&stable_cargo_build);

    let first_bootstrap = run_warp(
        cargo_built_warp,
        &output,
        &cache,
        workspace_root,
        &cargo_target_directory,
    );
    assert_success(&first_bootstrap);
    assert!(
        output_text(&first_bootstrap).contains("Building ")
            && output_text(&first_bootstrap).contains("warp-cli::warp"),
        "{}",
        output_text(&first_bootstrap)
    );
    assert!(output.is_file());
    assert_success(&Command::new(&output).arg("--version").output().unwrap());

    let warp_built_warp = run_warp(
        &output,
        &output,
        &cache,
        workspace_root,
        &cargo_target_directory,
    );
    assert_success(&warp_built_warp);
    assert!(
        output_text(&warp_built_warp).contains("OK warp-cli::warp ")
            && output_text(&warp_built_warp).contains("(cached)"),
        "{}",
        output_text(&warp_built_warp)
    );
    assert!(
        output_text(&warp_built_warp).contains("FINISHED 7 targets"),
        "{}",
        output_text(&warp_built_warp)
    );
    assert_success(&Command::new(&output).arg("--version").output().unwrap());
}

fn run_warp(
    binary: impl AsRef<Path>,
    output: &Path,
    cache: &Path,
    workspace_root: &Path,
    cargo_target_directory: &Path,
) -> Output {
    Command::new(binary.as_ref())
        .current_dir(workspace_root)
        .env("CARGO_TARGET_DIR", cargo_target_directory)
        .arg("build")
        // Exercise the full Rust workspace without making Rust-only tests depend on Bun.
        .args([
            "./crates/warp-cli",
            "./crates/warp-server",
            "./crates/warp-protocol",
            "./crates/warp-build-engine",
            "./crates/warp-build-language",
            "./crates/warp-rules",
            "./crates/warp-store",
        ])
        .arg("--output")
        .arg(output)
        .arg("--cache")
        .arg(cache)
        .output()
        .unwrap()
}

fn assert_success(output: &Output) {
    assert!(output.status.success(), "{}", output_text(output));
}

fn output_text(output: &Output) -> String {
    format!(
        "{}\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    )
}
