use crate::error::{Result, WarpError};
use std::path::{Path, PathBuf};
use std::process::Command;

pub fn default_cache_directory() -> Result<PathBuf> {
    let directories = directories::BaseDirs::new()
        .ok_or_else(|| WarpError::new("cannot determine this user's cache directory"))?;
    Ok(directories.cache_dir().join("warp"))
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BuildRequest {
    pub entry: PathBuf,
    pub output: PathBuf,
    pub cache: Option<PathBuf>,
    pub edition: Edition,
    pub target: TargetTriple,
    pub profile: BuildProfile,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Edition {
    E2015,
    E2018,
    E2021,
    E2024,
}

impl Edition {
    pub fn parse(value: &str) -> Result<Self> {
        match value {
            "2015" => Ok(Self::E2015),
            "2018" => Ok(Self::E2018),
            "2021" => Ok(Self::E2021),
            "2024" => Ok(Self::E2024),
            _ => Err(WarpError::new(format!(
                "unsupported Rust edition {value:?}; expected 2015, 2018, 2021, or 2024"
            ))),
        }
    }

    pub fn as_str(self) -> &'static str {
        match self {
            Self::E2015 => "2015",
            Self::E2018 => "2018",
            Self::E2021 => "2021",
            Self::E2024 => "2024",
        }
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub enum BuildProfile {
    #[default]
    Debug,
    Release,
}

impl BuildProfile {
    pub fn parse(value: &str) -> Result<Self> {
        match value {
            "debug" => Ok(Self::Debug),
            "release" => Ok(Self::Release),
            _ => Err(WarpError::new(format!(
                "unsupported build profile {value:?}; expected debug or release"
            ))),
        }
    }

    pub fn as_str(self) -> &'static str {
        match self {
            Self::Debug => "debug",
            Self::Release => "release",
        }
    }

    pub fn rustc_arguments(self) -> &'static [&'static str] {
        match self {
            Self::Debug => &["-C", "debuginfo=2"],
            Self::Release => &[
                "-C",
                "opt-level=3",
                "-C",
                "debug-assertions=off",
                "-C",
                "overflow-checks=off",
            ],
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TargetTriple(String);

impl TargetTriple {
    pub fn parse(value: &str) -> Result<Self> {
        let valid = !value.is_empty()
            && value
                .bytes()
                .all(|byte| byte.is_ascii_alphanumeric() || byte == b'-' || byte == b'_')
            && value
                .bytes()
                .next()
                .is_some_and(|byte| byte.is_ascii_alphanumeric())
            && value
                .bytes()
                .next_back()
                .is_some_and(|byte| byte.is_ascii_alphanumeric());
        if !valid {
            return Err(WarpError::new(format!(
                "invalid Rust target triple {value:?}; expected an alphanumeric target triple"
            )));
        }
        Ok(Self(value.to_string()))
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

pub fn rustc_flags(target: &TargetTriple) -> Vec<String> {
    let mut arguments = Vec::new();
    if let Some(linker) = std::env::var_os(target_environment_name(target, "LINKER")) {
        arguments.extend([
            "-C".to_string(),
            format!("linker={}", linker.to_string_lossy()),
        ]);
    }
    if let Some(flags) = std::env::var_os("CARGO_ENCODED_RUSTFLAGS") {
        arguments.extend(
            flags
                .to_string_lossy()
                .split('\u{1f}')
                .filter(|flag| !flag.is_empty())
                .map(str::to_owned),
        );
    } else if let Some(flags) = std::env::var_os("RUSTFLAGS") {
        arguments.extend(
            flags
                .to_string_lossy()
                .split_whitespace()
                .map(str::to_owned),
        );
    }
    if let Some(flags) = std::env::var_os(target_environment_name(target, "RUSTFLAGS")) {
        arguments.extend(
            flags
                .to_string_lossy()
                .split_whitespace()
                .map(str::to_owned),
        );
    }
    arguments
}

pub fn rustc_output_filename(
    compiler: &Path,
    target: &TargetTriple,
    crate_name: &str,
    crate_type: &str,
) -> Result<String> {
    let output = Command::new(compiler)
        .args([
            "--print",
            "file-names",
            "--crate-name",
            crate_name,
            "--crate-type",
            crate_type,
        ])
        .arg("--target")
        .arg(target.as_str())
        .arg("-")
        .stdin(std::process::Stdio::null())
        .output()
        .map_err(|error| WarpError::io("query rustc target artifact name", error))?;
    if !output.status.success() {
        return Err(WarpError::new(format!(
            "rustc could not determine the artifact name for target {}: {}",
            target.as_str(),
            String::from_utf8_lossy(&output.stderr)
        )));
    }
    let output = String::from_utf8(output.stdout).map_err(|error| {
        WarpError::new(format!("rustc returned a non-UTF-8 artifact name: {error}"))
    })?;
    let filename = output
        .lines()
        .next()
        .ok_or_else(|| WarpError::new("rustc returned no artifact filename"))?
        .trim();
    if filename.is_empty()
        || Path::new(filename)
            .file_name()
            .and_then(|name| name.to_str())
            != Some(filename)
    {
        return Err(WarpError::new(format!(
            "rustc returned an invalid artifact filename {filename:?}"
        )));
    }
    Ok(filename.to_string())
}

pub fn target_environment_name(target: &TargetTriple, suffix: &str) -> String {
    let target = target
        .as_str()
        .replace(['-', '.'], "_")
        .to_ascii_uppercase();
    format!("CARGO_TARGET_{target}_{suffix}")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn target_triples_are_safe_path_components() {
        assert_eq!(
            TargetTriple::parse("aarch64-apple-darwin")
                .unwrap()
                .as_str(),
            "aarch64-apple-darwin"
        );
        for invalid in ["", "-aarch64", "aarch64-", "../target", "aarch64 darwin"] {
            assert!(TargetTriple::parse(invalid).is_err(), "{invalid:?}");
        }
    }

    #[test]
    fn profiles_are_explicit_build_dimensions() {
        assert_eq!(BuildProfile::parse("debug").unwrap(), BuildProfile::Debug);
        assert_eq!(
            BuildProfile::parse("release").unwrap(),
            BuildProfile::Release
        );
        assert!(BuildProfile::parse("custom").is_err());
        assert_ne!(
            BuildProfile::Debug.rustc_arguments(),
            BuildProfile::Release.rustc_arguments()
        );
    }

    #[test]
    fn default_cache_uses_the_platform_user_cache_directory() {
        let expected = directories::BaseDirs::new()
            .unwrap()
            .cache_dir()
            .join("warp");
        assert_eq!(default_cache_directory().unwrap(), expected);
    }

    #[test]
    fn rustc_provides_target_specific_binary_suffixes() {
        let (compiler, _) = crate::fingerprint::rustc().unwrap();
        let target = TargetTriple::parse("wasm32-wasip1").unwrap();
        assert_eq!(
            rustc_output_filename(&compiler, &target, "tiny_tool", "bin").unwrap(),
            "tiny_tool.wasm"
        );
    }
}
