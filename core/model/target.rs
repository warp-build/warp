use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};
use thiserror::*;
use url::Url;

static ALIAS_ALL: &str = "@all";

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum Target {
    Alias(AliasTarget),
    Remote(RemoteTarget),
    Fs(FsTarget),
}

impl From<url::Url> for Target {
    fn from(url: url::Url) -> Self {
        Self::Remote(url.into())
    }
}

impl From<String> for Target {
    fn from(value: String) -> Self {
        value.as_str().into()
    }
}

impl From<&str> for Target {
    fn from(value: &str) -> Self {
        if value.starts_with('@') {
            Self::Alias(value.into())
        } else if let Ok(url) = url::Url::parse(value) {
            Self::Remote(url.into())
        } else {
            Self::Fs(value.into())
        }
    }
}

impl From<&Target> for Target {
    fn from(value: &Target) -> Self {
        value.to_owned()
    }
}

impl From<PathBuf> for Target {
    fn from(value: PathBuf) -> Self {
        Self::Fs(value.into())
    }
}

impl From<&Path> for Target {
    fn from(value: &Path) -> Self {
        Self::Fs(value.into())
    }
}

impl Default for Target {
    fn default() -> Self {
        Self::Alias(Default::default())
    }
}

impl ToString for Target {
    fn to_string(&self) -> String {
        match self {
            Target::Alias(a) => a.to_string(),
            Target::Remote(r) => r.to_string(),
            Target::Fs(f) => f.to_string(),
        }
    }
}

impl AsRef<Target> for Target {
    fn as_ref(&self) -> &Target {
        self
    }
}

impl std::str::FromStr for Target {
    type Err = TargetError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.starts_with('@') {
            return Ok(Self::Alias(AliasTarget {
                alias: s.to_string(),
            }));
        }

        if let Ok(url) = s.parse::<url::Url>() {
            return Ok(url.into());
        }

        Ok(Self::Fs(FsTarget {
            path: PathBuf::from(s), // TODO(@ostera): split by : to get the symbol name
        }))
    }
}

impl Target {
    pub fn is_remote(&self) -> bool {
        matches!(&self, Self::Remote(_))
    }

    pub fn is_all(&self) -> bool {
        matches!(
            &self,
            Self::Alias(AliasTarget {
                alias
            }) if alias == ALIAS_ALL
        )
    }

    pub fn url(&self) -> Option<Url> {
        match self {
            Target::Alias(_) => None,
            Target::Remote(r) => Some(r.url.parse::<Url>().unwrap()),
            Target::Fs(_) => None,
        }
    }
}

#[derive(Builder, Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct AliasTarget {
    alias: String,
}

impl AliasTarget {
    pub fn alias(&self) -> &str {
        self.alias.as_ref()
    }
}

impl Default for AliasTarget {
    fn default() -> Self {
        Self {
            alias: ALIAS_ALL.to_string(),
        }
    }
}

impl From<&str> for AliasTarget {
    fn from(value: &str) -> Self {
        Self {
            alias: value.to_string(),
        }
    }
}

impl ToString for AliasTarget {
    fn to_string(&self) -> String {
        self.alias.clone()
    }
}

#[derive(
    Builder, Default, Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
#[builder(build_fn(error = "TargetError"))]
pub struct RemoteTarget {
    #[builder(setter(into))]
    url: String,

    #[builder(default, setter(into, strip_option))]
    tricorder_url: Option<String>,

    #[builder(default, setter(into, strip_option))]
    subpath: Option<PathBuf>,
}

impl RemoteTarget {
    pub fn builder() -> RemoteTargetBuilder {
        RemoteTargetBuilder::default()
    }

    pub fn url(&self) -> Url {
        self.url.parse::<Url>().unwrap()
    }

    pub fn tricorder_url(&self) -> Option<Url> {
        self.tricorder_url
            .as_ref()
            .map(|url| url.clone().parse::<Url>().unwrap())
    }

    pub fn subpath(&self) -> Option<&PathBuf> {
        self.subpath.as_ref()
    }
}

impl ToString for RemoteTarget {
    fn to_string(&self) -> String {
        self.url.clone()
    }
}

impl From<url::Url> for RemoteTarget {
    fn from(url: url::Url) -> Self {
        Self {
            url: url.to_string(),
            tricorder_url: None,
            subpath: None,
        }
    }
}

#[derive(Builder, Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct FsTarget {
    path: PathBuf,
}

impl FsTarget {
    pub fn path(&self) -> &PathBuf {
        &self.path
    }
}

impl Default for FsTarget {
    fn default() -> Self {
        Self {
            path: PathBuf::from("."),
        }
    }
}

impl ToString for FsTarget {
    fn to_string(&self) -> String {
        self.path.to_string_lossy().to_string()
    }
}

impl From<&str> for FsTarget {
    fn from(value: &str) -> Self {
        Self { path: value.into() }
    }
}

impl From<PathBuf> for FsTarget {
    fn from(value: PathBuf) -> Self {
        Self { path: value }
    }
}

impl From<&Path> for FsTarget {
    fn from(value: &Path) -> Self {
        Self {
            path: value.to_path_buf(),
        }
    }
}

#[derive(Error, Debug)]
pub enum TargetError {
    #[error(transparent)]
    BuilderError(derive_builder::UninitializedFieldError),
}

impl From<derive_builder::UninitializedFieldError> for TargetError {
    fn from(value: derive_builder::UninitializedFieldError) -> Self {
        TargetError::BuilderError(value)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    impl quickcheck::Arbitrary for Target {
        fn arbitrary(g: &mut quickcheck::Gen) -> Self {
            let fs = Self::Fs(FsTarget::arbitrary(g));
            let remote = Self::Remote(RemoteTarget::arbitrary(g));
            let alias = Self::Alias(AliasTarget::arbitrary(g));
            g.choose(&[fs, remote, alias]).unwrap().to_owned()
        }
    }

    impl quickcheck::Arbitrary for FsTarget {
        fn arbitrary(g: &mut quickcheck::Gen) -> Self {
            Self {
                path: std::path::PathBuf::arbitrary(g),
            }
        }
    }

    impl quickcheck::Arbitrary for AliasTarget {
        fn arbitrary(g: &mut quickcheck::Gen) -> Self {
            Self {
                alias: format!("@{}", String::arbitrary(g)),
            }
        }
    }

    impl quickcheck::Arbitrary for RemoteTarget {
        fn arbitrary(g: &mut quickcheck::Gen) -> Self {
            Self {
                url: format!(
                    "https://{}",
                    std::path::PathBuf::arbitrary(g).to_string_lossy()
                ),
                tricorder_url: None,
                subpath: None,
            }
        }
    }
}
