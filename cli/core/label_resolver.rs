use super::*;
use thiserror::*;

#[derive(Debug)]
pub struct LabelResolver {
    store: LabelStore,
}

#[derive(Error, Debug)]
pub enum LabelResolverError {
    #[error(transparent)]
    LabelStoreError(LabelStoreError),

    #[error("Could not resolve label: {0:?}")]
    CouldNotResolveLabel(Label),

    #[error("Could not find target: {0:?}")]
    TargetNotFound(Label),

    #[error(transparent)]
    BuildfileError(BuildfileError),
}

impl LabelResolver {
    pub fn new() -> Self {
        Self { store: LabelStore }
    }

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

        let buildfile = Buildfile::from_file(&path)
            .await
            .map_err(LabelResolverError::BuildfileError)?;

        buildfile
            .targets
            .iter()
            .find(|t| t.label == *label)
            .ok_or_else(|| LabelResolverError::TargetNotFound(label.clone()))
            .cloned()
    }
}

mod tests {
    /* TODO(@ostera): finish these :)
    use super::*;

    #[test]
    async fn label_resolver_finds_a_target() {
        let store = LabelStore;
        let resolver = LabelResolver::new(store);

        let label = Label::new("https://hello.world/pkg");
        let target = resolver.resolve(&label).await.unwrap();

        assert!(false);
    }
    */
}
