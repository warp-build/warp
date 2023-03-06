use std::path::PathBuf;
use thiserror::Error;

pub struct RuleStore;

impl RuleStore {
    /// Get a Rule by name.
    pub async fn get<N>(&self, name: N) -> Result<PathBuf, RuleStoreError>
    where
        N: AsRef<str>,
    {
        unimplemented!()
    }
}

#[derive(Debug, Error)]
pub enum RuleStoreError {}
