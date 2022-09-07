use super::*;
use std::path::PathBuf;
use thiserror::*;

#[derive(Debug)]
pub struct LabelStore;

#[derive(Error, Debug)]
pub enum LabelStoreError {}

impl LabelStore {
    pub async fn fetch(&self, label: &Label) -> Result<Option<PathBuf>, LabelStoreError> {
        if let Some(path) = self.find_in_disk(&label).await? {
            return Ok(Some(path));
        }

        self.download(label).await?;
        self.find_in_disk(label).await
    }

    async fn find_in_disk(&self, label: &Label) -> Result<Option<PathBuf>, LabelStoreError> {
        todo!()
    }

    async fn download(&self, label: &Label) -> Result<Option<PathBuf>, LabelStoreError> {
        todo!()
    }
}

mod tests {}
