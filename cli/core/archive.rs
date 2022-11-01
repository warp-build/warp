use super::*;
use serde::{Deserialize, Serialize};

#[derive(Builder, Debug, Clone, Serialize, Deserialize)]
pub struct Archive {
    pub url: url::Url,

    #[serde(default)]
    pub checksum: String,

    #[serde(default)]
    pub prefix: String,

    #[serde(default)]
    pub strip_prefix: Option<String>,
}

impl From<Archive> for RemoteWorkspaceConfig {
    fn from(a: Archive) -> Self {
        Self::UrlWorkspace {
            url: a.url,
            sha1: a.checksum,
            prefix: a.prefix,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DependencyArchiveResolution {
    #[serde(default)]
    pub version: usize,

    pub archive: Archive,
}
