use std::path::PathBuf;

use super::*;

pub struct LabelResolver {
    store: LabelStore,
}

pub enum LabelResolverError {
    LabelStoreError(LabelStoreError),
    CouldNotResolveLabel(Label),
    TargetNotFound(PathBuf, Label),
    BuildfileError(BuildfileError),
}

impl LabelResolver {
    pub async fn resolve(&self, label: &Label) -> Result<Target, LabelResolverError> {
        let path = if label.is_remote() {
            self.store
                .fetch(&label)
                .await
                .map_err(LabelResolverError::LabelStoreError)?
                .ok_or_else(|| LabelResolverError::CouldNotResolveLabel(label.clone()))
        } else {
            Ok(label.path())
        }?;

        let buildfile = Buildfile2::from_file(&path)
            .await
            .map_err(LabelResolverError::BuildfileError)?;

        buildfile
            .targets
            .iter()
            .find(|t| *t.label() == *label)
            .ok_or_else(|| LabelResolverError::TargetNotFound(path, label.clone()))?
    }
}

mod tests {}
