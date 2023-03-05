use super::TargetId;
use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};
use thiserror::*;

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
            deps: vec![],
        }))
    }
}

impl Target {
    pub fn is_all(&self) -> bool {
        matches!(
            &self,
            Self::Alias(AliasTarget {
                alias
            }) if alias == ALIAS_ALL
        )
    }

    pub fn deps(&self) -> &[TargetId] {
        match self {
            Target::Alias(_) => &[],
            Target::Remote(_) => &[],
            Target::Fs(f) => f.deps(),
        }
    }
}

#[derive(Builder, Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct AliasTarget {
    alias: String,
}

impl Default for AliasTarget {
    fn default() -> Self {
        Self {
            alias: ALIAS_ALL.to_string(),
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
pub struct RemoteTarget {
    url: String,
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
        }
    }
}

#[derive(Builder, Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct FsTarget {
    path: PathBuf,
    #[serde(skip)]
    deps: Vec<TargetId>,
}

impl FsTarget {
    pub fn path(&self) -> &PathBuf {
        &self.path
    }

    pub fn deps(&self) -> &[TargetId] {
        &self.deps
    }
}

impl Default for FsTarget {
    fn default() -> Self {
        Self {
            path: PathBuf::from("."),
            deps: Default::default(),
        }
    }
}

impl ToString for FsTarget {
    fn to_string(&self) -> String {
        self.path.to_string_lossy().to_string()
    }
}

impl From<&Path> for FsTarget {
    fn from(value: &Path) -> Self {
        Self {
            path: value.to_path_buf(),
            deps: Default::default(),
        }
    }
}

#[derive(Error, Debug)]
pub enum TargetError {}

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
                deps: Default::default(),
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
                    std::path::PathBuf::arbitrary(g)
                        .to_string_lossy()
                        .to_string()
                ),
            }
        }
    }
}
