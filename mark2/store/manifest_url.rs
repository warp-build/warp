use super::{StoreError, DEFAULT_WARP_STORE_HOST};
use std::convert::TryFrom;
use url::Url;

#[derive(Debug, Clone)]
pub struct ManifestUrl(Url);

impl ManifestUrl {
    #[cfg(test)]
    pub fn new(url: Url) -> Self {
        Self(url)
    }

    pub fn url(&self) -> &Url {
        &self.0
    }
}

impl TryFrom<Url> for ManifestUrl {
    type Error = StoreError;

    fn try_from(url: Url) -> Result<Self, Self::Error> {
        if let Some(host) = url.host() {
            let is_installable_url = host.to_string().starts_with(DEFAULT_WARP_STORE_HOST);
            if is_installable_url {
                return Ok(Self(url));
            }
        }
        Err(StoreError::UrlIsNotInstallable { url })
    }
}
