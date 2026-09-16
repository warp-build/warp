use thiserror::Error;

static CRATES_API_URL_STR: &str = "https://crates.io/api/v1";
static CRATES_API_URL: once_cell::sync::Lazy<url::Url> =
    once_cell::sync::Lazy::new(|| url::Url::parse(CRATES_API_URL_STR).unwrap());

pub struct Crates;

impl Crates {
    pub fn new() -> Self {
        Self
    }

    pub async fn download_url(&self, name: &str, version: &str) -> Result<url::Url, CratesError> {
        let mut url = CRATES_API_URL.clone();
        {
            let mut path = url.path_segments_mut().unwrap();
            path.extend(&["crates", name, version, "download"]);
        };
        Ok(url)
    }
}

#[derive(Error, Debug)]
pub enum CratesError {}
