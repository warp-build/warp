use crate::error::{Result, WarpError};
use std::path::PathBuf;
use std::process::Command;

pub(super) struct BunToolchain {
    pub(super) executable: PathBuf,
    pub(super) version: String,
}

impl BunToolchain {
    pub(super) fn resolve() -> Result<Self> {
        let executable = PathBuf::from("bun");
        let output = Command::new(&executable)
            .arg("--version")
            .output()
            .map_err(|error| {
                if error.kind() == std::io::ErrorKind::NotFound {
                    WarpError::new(
                        "Bun is required to build this package; install the version pinned in package.json from https://bun.sh",
                    )
                } else {
                    WarpError::io("query Bun toolchain version", error)
                }
            })?;
        if !output.status.success() {
            return Err(WarpError::new(format!(
                "Bun version query failed with {}: {}",
                output.status,
                String::from_utf8_lossy(&output.stderr).trim()
            )));
        }
        let version = String::from_utf8(output.stdout)
            .map_err(|error| WarpError::new(format!("Bun returned invalid UTF-8: {error}")))?
            .trim()
            .to_string();
        if version.is_empty() {
            return Err(WarpError::new("Bun returned an empty version string"));
        }
        Ok(Self {
            executable,
            version,
        })
    }
}
