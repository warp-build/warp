use dashmap::DashMap;
use std::path::PathBuf;
use thiserror::*;

#[derive(Default)]
pub struct RuleStore {
    loaded_rules: DashMap<String, ()>,
}

#[derive(Error, Debug)]
pub enum RuleStoreError {}

impl RuleStore {
    pub fn new() -> Self {
        Self::default()
    }

    pub async fn get(&self, _name: &str) -> Result<PathBuf, RuleStoreError> {
        /*
        if self.loaded_rules.has_key(&name) {
            return Ok(());
        }

        if let Some(path) = self.find_in_workspace(&name).await? {
            return self.load(path).await;
        }

        if let Some(path) = self.find_in_store(&name).await? {
            return self.load(path).await;
        }

        let path = self.fetch(&name).await?;

        self.load(path).await
        */
        todo!()
    }
}

mod tests {}
