use super::*;
use dashmap::DashMap;
use futures::stream::TryStreamExt;
use futures::StreamExt;
use std::path::PathBuf;
use thiserror::*;
use tokio::fs;
use tokio::io::AsyncWriteExt;

const DEFAULT_RULE_URL: &str = "https://pkgs.warp.build/rules";

#[derive(Debug, Default)]
pub struct RuleStore {
    local_rules_root: PathBuf,
    global_rules_root: PathBuf,
    loaded_rules: DashMap<String, PathBuf>,
    client: reqwest::Client,
}

#[derive(Error, Debug)]
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

impl RuleStore {
    pub fn new(workspace: &Workspace) -> Self {
        Self {
            local_rules_root: workspace.paths.local_rules_root.clone(),
            global_rules_root: workspace.paths.global_rules_root.clone(),
            loaded_rules: DashMap::default(),
            client: reqwest::Client::new(),
        }
    }

    #[tracing::instrument(name = "RuleStore::get", skip(self))]
    pub async fn get(&self, name: &str) -> Result<(PathBuf, String), RuleStoreError> {
        // Optimization to avoid doing IO every time
        if let Some(path) = self.loaded_rules.get(name) {
            return Ok((path.clone(), name.to_string()));
        }

        if let Some(path) = self.find_in_workspace(name).await? {
            self.save(name, &path);
            return Ok((path, name.to_string()));
        }

        let normalized_name = self.normalize_name(name);
        if let Some(path) = self.fetch(&normalized_name).await? {
            self.save(&normalized_name, &path);
            return Ok((path, normalized_name.to_string()));
        }

        Err(RuleStoreError::CouldNotFindRule {
            name: name.to_string(),
            normalized_name,
            loaded_rules: self.loaded_rules.clone(),
        })
    }

    fn normalize_name(&self, name: &str) -> String {
        let url = url::Url::parse(name);
        if url.is_ok() {
            name.to_string()
        } else {
            format!("{}/{}", DEFAULT_RULE_URL, name)
        }
    }

    #[tracing::instrument(name = "RuleStore::save", skip(self))]
    fn save(&self, name: &str, path: &PathBuf) {
        self.loaded_rules.insert(name.to_string(), path.clone());
    }

    #[tracing::instrument(name = "RuleStore::find_in_workspace", skip(self))]
    async fn find_in_workspace(&self, name: &str) -> Result<Option<PathBuf>, RuleStoreError> {
        let rule_path = self.local_rules_root.join(name).with_extension("js");

        if let Ok(_meta) = fs::metadata(&rule_path).await {
            Ok(Some(rule_path))
        } else {
            Ok(None)
        }
    }

    #[tracing::instrument(name = "RuleStore::fetch", skip(self))]
    async fn fetch(&self, name: &str) -> Result<Option<PathBuf>, RuleStoreError> {
        if let Some(path) = self.find_in_global_rules(name).await? {
            return Ok(Some(path));
        }
        self.download(name).await
    }

    #[tracing::instrument(name = "RuleStore::find_in_global_rules", skip(self))]
    async fn find_in_global_rules(&self, name: &str) -> Result<Option<PathBuf>, RuleStoreError> {
        let path = self._store_key(name);
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

    pub fn _store_key(&self, name: &str) -> PathBuf {
        let label = Label::new(name);
        self.global_rules_root
            .join(label.as_store_prefix())
            .join(label.name())
            .with_extension("js")
    }

    #[tracing::instrument(name = "RuleStore::download", skip(self))]
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
            let path = self._store_key(&name);

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
}

#[cfg(test)]
mod tests {
    use super::*;
}
