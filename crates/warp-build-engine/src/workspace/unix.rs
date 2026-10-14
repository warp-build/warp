use std::io;
use std::path::Path;

pub fn dynamic_library_prefix(crate_name: &str) -> String {
    format!("lib{crate_name}")
}

pub fn dynamic_library_suffix() -> &'static str {
    if cfg!(target_os = "macos") {
        ".dylib"
    } else {
        ".so"
    }
}

pub fn dynamic_library_extension() -> &'static str {
    dynamic_library_suffix().trim_start_matches('.')
}

pub fn native_loader_variable() -> &'static str {
    if cfg!(target_os = "macos") {
        "DYLD_LIBRARY_PATH"
    } else {
        "LD_LIBRARY_PATH"
    }
}

pub fn mark_executable(path: &Path) -> io::Result<()> {
    use std::os::unix::fs::PermissionsExt;

    let mut permissions = std::fs::metadata(path)?.permissions();
    if permissions.mode() & 0o111 == 0o111 {
        return Ok(());
    }
    permissions.set_mode(0o755);
    std::fs::set_permissions(path, permissions)
}

pub fn publish_file(temporary: &Path, output: &Path) -> io::Result<()> {
    std::fs::rename(temporary, output)
}
