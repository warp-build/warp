use std::io;
use std::path::Path;

pub fn dynamic_library_prefix(_crate_name: &str) -> String {
    String::new()
}

pub fn dynamic_library_suffix() -> &'static str {
    ".dll"
}

pub fn dynamic_library_extension() -> &'static str {
    "dll"
}

pub fn native_loader_variable() -> &'static str {
    "PATH"
}

pub fn mark_executable(_path: &Path) -> io::Result<()> {
    Ok(())
}

pub fn publish_file(temporary: &Path, output: &Path) -> io::Result<()> {
    if output.exists() {
        std::fs::remove_file(output)?;
    }
    std::fs::rename(temporary, output)
}
