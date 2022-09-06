use super::*;

pub struct LabelResolver {
    store: LabelStore,
}

impl LabelResolver {
    pub async fn resolve(&self, label: &Label) -> Result<Target, LabelResolverError> {
        let path = if label.is_remote() {
            self.store
                .fetch(&label)
                .await
                .map_err(LabelResolverError::LabelStoreError)?
        } else {
            label.path()
        };

        let buildfile = Buildfile::from_file(path)
            .await
            .map_err(LabelResolverError::BuildfileError)?;

        buildfile
            .targets
            .iter()
            .find(|t| *t.label() == *label)
            .ok_or_else(|| LabelResolverError::TargetNotFound(path, label))?
    }
}

mod tests {}
