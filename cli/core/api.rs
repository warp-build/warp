use super::*;
use reqwest::header::*;
use thiserror::Error;

pub const DEFAULT_API_URL: &str = "https://api.warp.build/v0";

#[derive(Error, Debug)]
pub enum ApiError {
    #[error(transparent)]
    Unknown(anyhow::Error),

    #[error(transparent)]
    UrlError(url::ParseError),

    #[error(transparent)]
    HeaderError(reqwest::header::InvalidHeaderValue),

    #[error(transparent)]
    ClientError(reqwest::Error),

    #[error(transparent)]
    RequestError(reqwest::Error),
}

#[derive(Debug, Clone)]
pub struct API {
    /// The URL this API instance points to
    pub url: url::Url,

    client: reqwest::Client,
}

impl Default for API {
    fn default() -> Self {
        Self {
            client: reqwest::Client::builder().gzip(false).build().unwrap(),
            url: DEFAULT_API_URL.parse().unwrap(),
        }
    }
}

impl API {
    pub fn from_workspace(workspace: &Workspace) -> API {
        API {
            url: workspace
                .remote_cache_url
                .clone()
                .unwrap_or_else(|| DEFAULT_API_URL.parse().unwrap()),
            ..API::default()
        }
    }

    #[tracing::instrument(name = "API::get_signed_url", skip(self))]
    pub async fn get_signed_url(&mut self, hash: &str) -> Result<url::Url, ApiError> {
        let url = format!("{}/artifact/{}.tar.gz", self.url, &hash);
        let response = self
            .client
            .post(url)
            .send()
            .await
            .map_err(ApiError::ClientError)?;

        let upload_url: std::collections::HashMap<String, String> =
            response.json().await.map_err(ApiError::RequestError)?;

        upload_url
            .get("signed_url")
            .unwrap()
            .parse()
            .map_err(ApiError::UrlError)
    }

    #[tracing::instrument(name = "API::upload_artifact", skip(self, contents))]
    pub async fn upload_artifact(&mut self, hash: &str, contents: &[u8]) -> Result<(), ApiError> {
        let upload_url = self.get_signed_url(&hash).await?;

        let mut headers = reqwest::header::HeaderMap::new();
        headers.insert("ACL", "public-read".parse().map_err(ApiError::HeaderError)?);
        headers.insert(
            CONTENT_TYPE,
            "application/octet-stream"
                .parse()
                .map_err(ApiError::HeaderError)?,
        );

        let request = self
            .client
            .put(upload_url)
            .headers(headers)
            .body(contents.to_vec())
            .build()
            .map_err(ApiError::RequestError)?;

        let _ = self
            .client
            .execute(request)
            .await
            .map_err(ApiError::RequestError)?;

        Ok(())
    }
}
