use super::*;

pub struct RuleLoader {
    store: RuleStore,
    loaded_rules: DashMap<String, ()>,
}

impl RuleLoader {
    pub async fn ensure_loaded(&self, name: &str) -> Result<(), RuleLoaderError> {
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
    }
}

mod tests {}
