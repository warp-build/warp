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

#[test]
fn cargo_built_warp_builds_warp_and_then_reuses_the_workspace_cache() {
    let directory = TestDirectory::new();
    let output = directory.0.join("debug/warp");
    let cache = directory.0.join("cache");
    let cargo_built_warp = env!("CARGO_BIN_EXE_warp");

    let first_bootstrap = run_warp(cargo_built_warp, &output, &cache);
    assert_success(&first_bootstrap);
    assert!(
        output_text(&first_bootstrap).contains("built 6 targets"),
        "{}",
        output_text(&first_bootstrap)
    );
    assert!(output.is_file());
    assert_success(&Command::new(&output).arg("--version").output().unwrap());

    let warp_built_warp = run_warp(&output, &output, &cache);
    assert_success(&warp_built_warp);
    assert!(
        output_text(&warp_built_warp).contains("cached 6 targets"),
        "{}",
        output_text(&warp_built_warp)
    );
    assert_success(&Command::new(&output).arg("--version").output().unwrap());
}

fn run_warp(binary: impl AsRef<Path>, output: &Path, cache: &Path) -> Output {
    Command::new(binary.as_ref())
        .current_dir(env!("CARGO_MANIFEST_DIR"))
        .arg("build")
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
