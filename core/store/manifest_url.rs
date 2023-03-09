use url::Url;

#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
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

impl From<Url> for ManifestUrl {
    fn from(url: Url) -> Self {
        Self(url)
    }
}
