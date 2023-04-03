use crate::sync::{Arc, Mutex};
use crate::Config;
use dashmap::DashMap;
use futures::stream::TryStreamExt;
use futures::StreamExt;
use std::path::{Path, PathBuf};
use thiserror::Error;
use tokio::fs;
use tokio::io::AsyncWriteExt;
use tracing::instrument;
use url::Url;

#[derive(Debug, Clone)]
pub struct RuleStore {
    global_rules_root: PathBuf,
    loaded_rules: DashMap<String, PathBuf>,
    public_rule_store_url: Url,
    client: reqwest::Client,
    _lock: Arc<Mutex<()>>,
}

impl RuleStore {
    pub fn new(config: &Config) -> Self {
        Self {
            global_rules_root: config.rule_store_root().clone(),
            loaded_rules: DashMap::default(),
            public_rule_store_url: config.public_rule_store_url().clone(),
            client: config.http_client().clone(),
            _lock: Arc::new(Mutex::new(())),
        }
    }

    /// Get a Rule by name.
    pub async fn get(&self, name: &str) -> Result<PathBuf, RuleStoreError> {
        let normalized_name = self.normalize_name(name);

        if let Some(path) = self.loaded_rules.get(&normalized_name) {
            return Ok(path.to_path_buf());
        }

        if let Some(path) = self.fetch(&normalized_name).await? {
            self.save(&normalized_name, &path);
            return Ok(path);
        }

        Err(RuleStoreError::CouldNotFindRule {
            name: name.to_string(),
            normalized_name: normalized_name.clone(),
            loaded_rules: self.loaded_rules.clone(),
        })
    }

    #[instrument(name = "RuleStore::fetch", skip(self))]
    async fn fetch(&self, name: &str) -> Result<Option<PathBuf>, RuleStoreError> {
        if let Some(path) = self.find_in_global_rules(name).await? {
            return Ok(Some(path));
        }
        self.download(name).await
    }

    #[instrument(name = "RuleStore::find_in_global_rules", skip(self), ret)]
    async fn find_in_global_rules(&self, name: &str) -> Result<Option<PathBuf>, RuleStoreError> {
        let path = self._global_rule_path(name);
        let meta = fs::metadata(&path)
            .await
            .map_err(|err| RuleStoreError::FileSystemError {
                name: name.to_string(),
                path: path.clone(),
                err,
            });
        if meta.is_err() {
            Ok(None)
        } else {
            Ok(Some(path))
        }
    }

    #[instrument(name = "RuleStore::download", skip(self), ret)]
    async fn download(&self, name: &str) -> Result<Option<PathBuf>, RuleStoreError> {
        let name = if name.ends_with(".js") {
            name.to_string()
        } else {
            format!("{}.js", name)
        };

        let response = self.client.get(&name).send().await.map_err(|err| {
            RuleStoreError::CouldNotDownload {
                name: name.clone(),
                err,
            }
        })?;

        if response.status().is_success() {
            let path = self._global_rule_path(&name);

            self.stream_response(response, path.clone())
                .await
                .map_err(|err| RuleStoreError::FileSystemError {
                    name: name.to_string(),
                    path: path.clone(),
                    err,
                })?;

            Ok(Some(path))
        } else {
            Ok(None)
        }
    }

    async fn stream_response(
        &self,
        response: reqwest::Response,
        path: PathBuf,
    ) -> Result<(), std::io::Error> {
        let _lock = self._lock.lock();

        let mut byte_stream = response
            .bytes_stream()
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e));

        fs::create_dir_all(&path.parent().unwrap()).await?;

        let mut outfile = fs::File::create(&path).await?;
        while let Some(chunk) = byte_stream.next().await {
            outfile.write_all_buf(&mut chunk?).await?;
        }
        Ok(())
    }

    fn save(&self, name: &str, path: &Path) {
        self.loaded_rules
            .insert(name.to_string(), path.to_path_buf());
    }

    #[instrument(name = "RuleStore::normalize_name", skip(self), ret)]
    pub fn normalize_name(&self, name: &str) -> String {
        let url = url::Url::parse(name);
        if url.is_ok() {
            name.to_string()
        } else {
            let mut url = self.public_rule_store_url.clone();
            url.path_segments_mut().unwrap().push(name);
            url.to_string()
        }
    }

    /// NOTE(@ostera): when normalizing the name to use it in paths, we need to drop the
    /// protocol :// so `http://hello.world/a` becomes `http/hello.world/a` which is a valid
    /// path name.
    #[instrument(name = "RuleStore::_global_rule_path", skip(self), ret)]
    fn _global_rule_path(&self, name: &str) -> PathBuf {
        let name = name.replace("://", "/");
        self.global_rules_root.join(name).with_extension("js")
    }
}

#[derive(Debug, Error)]
pub enum RuleStoreError {
    #[error("Could not find rule named {name:?} (normalized name: {normalized_name:?}) in: {loaded_rules:#?}")]
    CouldNotFindRule {
        name: String,
        normalized_name: String,
        loaded_rules: DashMap<String, PathBuf>,
    },

    #[error("Could not read rule {name:?} at {path:?}, due to: {err:?}")]
    FileSystemError {
        name: String,
        path: PathBuf,
        err: std::io::Error,
    },

    #[error("Could not download rule {name:?} due to: {err:?}")]
    CouldNotDownload { name: String, err: reqwest::Error },
}

#[cfg(test)]
mod tests {
    use url::Url;

    use super::*;

    #[test]
    fn initializes_store_with_rules_root() {
        let store_root = assert_fs::TempDir::new().unwrap();

        let config = Config::builder()
            .rule_store_root(store_root.path().to_path_buf())
            .build()
            .unwrap();

        let rs = RuleStore::new(&config);

        assert_eq!(&rs.global_rules_root, config.rule_store_root())
    }

    #[tokio::test]
    async fn gets_rule_from_name() {
        let store_root = assert_fs::TempDir::new().unwrap();

        let config = Config::builder()
            .rule_store_root(store_root.path().to_path_buf())
            .public_rule_store_url(mockito::server_url().parse::<Url>().unwrap())
            .build()
            .unwrap();

        let m = mockito::mock("GET", "/dummy_rule.js")
            .with_status(200)
            .with_body(include_bytes!("./fixtures/dummy_rule.js"))
            .create();

        let rs = RuleStore::new(&config);

        let rule_name = "dummy_rule";
        assert_eq!(
            rs.get(rule_name).await.unwrap(),
            *rs.loaded_rules.get(&rs.normalize_name(rule_name)).unwrap()
        );

        m.assert();
    }

    #[tokio::test]
    async fn does_not_hit_server_if_rule_is_loaded() {
        let store_root = assert_fs::TempDir::new().unwrap();

        let config = Config::builder()
            .rule_store_root(store_root.path().to_path_buf())
            .public_rule_store_url(mockito::server_url().parse::<Url>().unwrap())
            .build()
            .unwrap();

        let m = mockito::mock("GET", "/dummy_rule.js")
            .with_status(200)
            .with_body(include_bytes!("./fixtures/dummy_rule.js"))
            .expect(0)
            .create();

        let rs = RuleStore::new(&config);

        let rule_name = "dummy_rule.js";

        rs.loaded_rules
            .insert(rs.normalize_name(rule_name), PathBuf::new());

        assert_eq!(rs.get(rule_name).await.unwrap(), PathBuf::new());
        m.assert();
    }

    #[tokio::test]
    async fn fails_when_cannot_download_file() {
        let store_root = assert_fs::TempDir::new().unwrap();

        let config = Config::builder()
            .rule_store_root(store_root.path().to_path_buf())
            .public_rule_store_url(mockito::server_url().parse::<Url>().unwrap())
            .build()
            .unwrap();

        let m = mockito::mock("GET", "/unreachable_rule.js")
            .with_status(404)
            .expect(1)
            .create();

        let rs = RuleStore::new(&config);

        let rule_name = "unreachable_rule.js";

        assert_matches!(
            rs.get(rule_name).await.unwrap_err(),
            RuleStoreError::CouldNotFindRule { .. }
        );
        m.assert();
    }
}
