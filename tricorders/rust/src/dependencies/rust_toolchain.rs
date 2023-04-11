use serde::{Deserialize, Deserializer};
use std::path::Path;
use thiserror::Error;

#[derive(Default, Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Deserialize)]
pub struct RustToolchain {
    #[serde(default)]
    toolchain: Toolchain,
}

impl RustToolchain {
    pub async fn from_path<P: AsRef<Path>>(path: P) -> Result<Self, RustToolchainError> {
        let toml = tokio::fs::read_to_string(path.as_ref()).await?;
        Self::from_str(&toml)
    }

    pub fn from_str(toml: &str) -> Result<Self, RustToolchainError> {
        Ok(toml::from_str(toml)?)
    }

    pub fn toolchain(&self) -> &Toolchain {
        &self.toolchain
    }
}

#[derive(Default, Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Deserialize)]
pub struct Toolchain {
    #[serde(default)]
    channel: Channel,
    #[serde(default)]
    components: Vec<String>,
    #[serde(default)]
    targets: Vec<String>,
    #[serde(default)]
    profile: Profile,
}

impl Toolchain {
    pub fn channel(&self) -> &Channel {
        &self.channel
    }

    pub fn components(&self) -> &[String] {
        self.components.as_ref()
    }

    pub fn targets(&self) -> &[String] {
        self.targets.as_ref()
    }

    pub fn profile(&self) -> &Profile {
        &self.profile
    }
}

#[derive(Default, Debug, Eq, PartialEq, Ord, PartialOrd, Clone)]
pub enum Channel {
    #[default]
    Stable,
    Beta,
    Nightly,
    Other(String),
}

impl<'de> Deserialize<'de> for Channel {
    fn deserialize<D>(de: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let channel = String::deserialize(de)?;
        Ok(match channel.as_str() {
            "stable" => Self::Stable,
            "beta" => Self::Beta,
            "nightly" => Self::Nightly,
            _other => Self::Other(channel),
        })
    }
}

#[derive(Default, Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Profile {
    #[default]
    Minimal,
}

#[derive(Error, Debug)]
pub enum RustToolchainError {
    #[error(transparent)]
    TomlError(toml::de::Error),

    #[error(transparent)]
    FileError(std::io::Error),
}

impl From<toml::de::Error> for RustToolchainError {
    fn from(value: toml::de::Error) -> Self {
        RustToolchainError::TomlError(value)
    }
}

impl From<std::io::Error> for RustToolchainError {
    fn from(value: std::io::Error) -> Self {
        RustToolchainError::FileError(value)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_default_toolchain() {
        let rust_toolchain = r#"
"#;

        let rust_toolchain = toml::from_str::<RustToolchain>(rust_toolchain).unwrap();

        assert_eq!(rust_toolchain.toolchain.channel, Channel::Stable);
        assert_eq!(rust_toolchain.toolchain.components.len(), 0);
        assert!(rust_toolchain.toolchain.components.is_empty());
        assert!(rust_toolchain.toolchain.targets.is_empty());
        assert_eq!(rust_toolchain.toolchain.profile, Profile::Minimal);
    }

    #[test]
    fn parse_toolchain_file() {
        let rust_toolchain = RustToolchain::from_str(
            r#"

[toolchain]
channel = "stable"
components = ["rustfmt", "clippy", "llvm-tools"]
targets = [
  "aarch64-apple-darwin",
  "x86_64-apple-darwin",
  "x86_64-unknown-linux-gnu",
  "aarch64-unknown-linux-gnu",
]
profile = "minimal"
"#,
        )
        .unwrap();

        assert_eq!(rust_toolchain.toolchain.channel, Channel::Stable);
        assert_eq!(rust_toolchain.toolchain.components.len(), 3);
        assert_eq!(
            rust_toolchain.toolchain.components,
            vec!["rustfmt", "clippy", "llvm-tools"]
        );
        assert_eq!(
            rust_toolchain.toolchain.targets,
            vec![
                "aarch64-apple-darwin",
                "x86_64-apple-darwin",
                "x86_64-unknown-linux-gnu",
                "aarch64-unknown-linux-gnu",
            ]
        );
        assert_eq!(rust_toolchain.toolchain.profile, Profile::Minimal);
    }
}
