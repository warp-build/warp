use super::*;
use serde::{Deserialize, Serialize};
use std::convert::TryFrom;
use thiserror::*;

#[derive(Builder, Debug, Clone, Serialize, Deserialize)]
pub struct Archive {
    pub url: url::Url,

    #[serde(default)]
    pub sha256: String,

    #[serde(default)]
    pub prefix: String,

    #[serde(default)]
    pub strip_prefix: Option<String>,
}

#[derive(Error, Debug)]
pub enum ArchiveError {
    #[error("Can't build an archive with an invalid URL: {url}, due to: {err:?}")]
    InvalidUrl { url: String, err: url::ParseError },
}

impl From<Archive> for RemoteWorkspaceConfig {
    fn from(a: Archive) -> Self {
        Self::UrlWorkspace {
            url: a.url,
            // FIXME(@ostera): we aren't using this just yet, but we should standardize on SHA256
            sha1: a.sha256,
            prefix: a.prefix,
        }
    }
}

impl TryFrom<proto::build::warp::Archive> for Archive {
    type Error = ArchiveError;

    fn try_from(archive: proto::build::warp::Archive) -> Result<Self, Self::Error> {
        let url: url::Url = archive
            .url
            .parse()
            .map_err(|err| ArchiveError::InvalidUrl {
                url: archive.url.clone(),
                err,
            })?;

        let strip_prefix = if archive.strip_prefix.is_empty() {
            None
        } else {
            Some(archive.strip_prefix.clone())
        };

        let sha256 = archive.sha256;

        Ok(Self {
            url,
            sha256,
            strip_prefix,
            prefix: "".into(),
        })
    }
}
