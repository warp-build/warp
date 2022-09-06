pub struct LabelStore;

impl LabelStore {
    pub fn fetch(&self, label: &Label) -> Result<Option<PathBuf>, LabelStoreError> {
        if let Some(path) = self.find_in_disk(&label).await? {
            return Some(path);
        }

        self.download(&label).await?;
        self.find_in_disk(&label).await
    }
}

mod tests {}
