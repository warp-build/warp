use assert_cmd::Command;
use std::path::PathBuf;

fn run(parts: &[&str]) -> String {
    let root = std::fs::canonicalize(PathBuf::from(".")).unwrap();
    let parts = parts.to_vec();
    let mut cmd = Command::cargo_bin(parts[0]).unwrap();
    cmd.env("RUST_BACKTRACE", "1");
    cmd.env("ZAP_LOG", "info");
    cmd.current_dir(std::fs::canonicalize(PathBuf::from("./tests/sample_project")).unwrap());
    cmd.arg("--quiet");
    cmd.args(&["--user", "warp-runner"]);
    cmd.args(&["--warp-home", "../_warp_home"]);
    for arg in &parts[1..] {
        cmd.arg(arg);
    }
    let bytes = cmd.assert().get_output().stdout.to_vec();

    std::env::set_current_dir(root).unwrap();

    String::from_utf8(bytes).unwrap()
}

#[test]
#[ignore]
pub fn warp_workspace_info() {
    insta::assert_snapshot!(run(&["warp", "workspace", "info"]))
}

#[test]
#[ignore]
pub fn warp_toolchains_list_active() {
    insta::assert_snapshot!(run(&["warp", "toolchains", "list-active"]))
}

#[test]
#[ignore]
pub fn warp_toolchains_list_archives() {
    insta::assert_snapshot!(run(&["warp", "toolchains", "list-archives"]))
}

#[test]
#[ignore]
pub fn warp_toolchains_list_available() {
    insta::assert_snapshot!(run(&["warp", "toolchains", "list-available"]))
}

#[test]
#[ignore]
pub fn warp_targets_list() {
    insta::assert_snapshot!(run(&["warp", "targets", "list"]))
}

#[test]
#[ignore]
pub fn warp_rules_list() {
    insta::assert_snapshot!(run(&["warp", "rules", "list"]))
}

#[test]
#[ignore]
pub fn warp_rules_dump_actions() {
    insta::assert_snapshot!(run(&["warp", "rules", "dump-actions", "//..."]))
}

#[test]
#[ignore]
pub fn warp_rules_dump_outputs() {
    insta::assert_snapshot!(run(&["warp", "rules", "dump-outputs", "//..."]))
}

#[test]
#[ignore]
pub fn warp_dep_graph_print() {
    // insta::assert_snapshot!(run(&["warp", "dep-graph", "print", "//..."]))
}

#[test]
#[ignore]
pub fn warp_cache_clear() {
    // insta::assert_snapshot!(run(&["warp", "cache", "clear", "//a:lib"]))
}

#[test]
#[ignore]
pub fn warp_build() {
    insta::assert_snapshot!(run(&["warp"]));
    insta::assert_snapshot!(run(&["warp", "build"]));
    insta::assert_snapshot!(run(&["warp", "build", "//..."]));
    insta::assert_snapshot!(run(&["warp", "build", "//a:lib"]));
}
