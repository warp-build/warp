use dashmap::DashMap;
use std::path::PathBuf;
use thiserror::*;

#[derive(Default)]
pub struct RuleStore {
    loaded_rules: DashMap<String, PathBuf>,
}

#[derive(Error, Debug)]
pub enum RuleStoreError {}

impl RuleStore {
    pub fn new() -> Self {
        Self::default()
    }

    pub async fn get(&self, name: &str) -> Result<PathBuf, RuleStoreError> {
        if let Some(path) = self.loaded_rules.get(&name) {
            return Ok(path.clone());
        }

        if let Some(path) = self.find_in_workspace(&name).await? {
            return Ok(path.clone());
        }

        if let Some(path) = self.find_in_store(&name).await? {
            return Ok(path.clone());
        }

        let path = self.fetch(&name).await?;
        self.load(path).await;
    }
}

mod tests {}
