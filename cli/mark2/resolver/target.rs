use serde::{Deserialize, Serialize};
use thiserror::*;

static ALIAS_ALL: &str = "@all";

#[derive(Error, Debug)]
pub enum TargetError {}

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
        if s.starts_with("@") {
            return Ok(Self::Alias(AliasTarget {
                alias: s.to_string(),
            }));
        }

        if let Ok(url) = s.parse::<url::Url>() {
            return Ok(url.into());
        }

        Ok(Self::Fs(FsTarget {
            path: s.to_string(),
            // TODO(@ostera): split by : to get the symbol name
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

#[derive(
    Builder, Default, Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct FsTarget {
    path: String,
}

impl ToString for FsTarget {
    fn to_string(&self) -> String {
        self.path.clone()
    }
}
