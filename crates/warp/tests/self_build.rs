use std::ffi::OsStr;
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
            std::env::temp_dir().join(format!("warp-self-build-test-{}-{id}", std::process::id()));
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
fn warp_builds_warp_and_the_result_reuses_its_own_cache() {
    let workspace = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let entry = workspace.join("crates/warp/src/main.rs");
    let directory = TestDirectory::new();
    let cache = directory.0.join("cache");
    let first_output = directory.0.join("first/warp");
    let second_output = directory.0.join("second/warp");

    let first = run_warp(env!("CARGO_BIN_EXE_warp"), &entry, &first_output, &cache);
    assert!(first.status.success(), "{}", output_text(&first));
    assert!(output_text(&first).contains("built ("));

    let second = run_warp(&first_output, &entry, &second_output, &cache);
    assert!(second.status.success(), "{}", output_text(&second));
    assert!(output_text(&second).contains("cached ("));
    assert!(second_output.is_file());
}

fn run_warp(binary: impl AsRef<OsStr>, entry: &Path, output: &Path, cache: &Path) -> Output {
    Command::new(binary)
        .arg("build")
        .arg(entry)
        .arg("--output")
        .arg(output)
        .arg("--cache")
        .arg(cache)
        .output()
        .unwrap()
}

fn output_text(output: &Output) -> String {
    format!(
        "{}\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    )
}
