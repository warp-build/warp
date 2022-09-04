use super::local_cache::*;
use super::remote_cache::*;
use super::*;
use std::path::PathBuf;
use tracing::*;

pub type CacheKey = String;

#[derive(Debug, Clone)]
pub struct Cache {
    workspace_prefix: String,
    local_cache: LocalCache,
    remote_cache: RemoteCache,
}

#[derive(Debug, Clone)]
pub enum CacheHitType {
    Miss(PathBuf),
    Hit(PathBuf),
}

impl Cache {
    #[tracing::instrument(name = "Cache::new", skip(workspace))]
    pub fn new(workspace: &Workspace) -> Self {
        Cache {
            local_cache: LocalCache::new(&workspace),
            remote_cache: RemoteCache::new(&workspace),
            workspace_prefix: workspace.paths.workspace_name.clone(),
        }
    }

    pub fn _cache_key(&self, hash: &str, label: &Label) -> CacheKey {
        if label.is_remote() {
            format!("{}-{}", label.as_cache_prefix(), hash)
        } else {
            format!("{}/{}", self.workspace_prefix, hash)
        }
    }

    pub fn cache_key(&self, node: &ComputedTarget) -> CacheKey {
        self._cache_key(&node.hash(), node.target.label())
    }

    pub fn cache_key_for_dep(&self, dep: &Dependency) -> CacheKey {
        self._cache_key(&dep.hash, &dep.label)
    }

    #[tracing::instrument(name = "Cache::save", skip(sandbox))]
    pub async fn save(&mut self, sandbox: &LocalSandbox) -> Result<(), anyhow::Error> {
        let node = sandbox.node();
        let cache_key = self.cache_key(&node);

        // NOTE(@ostera): see RFC0005
        let artifacts = if node.target.is_pinned() {
            sandbox.all_outputs().await
        } else {
            sandbox.outputs()
        };

        self.local_cache
            .save(&cache_key, &artifacts, &sandbox.root())
            .await?;

        /* FIXME(@ostera): to reenable this we need to rework the API
        let _ = self
            .remote_cache
            .save(&cache_key, &artifacts, &sandbox.root())
            .await;
 */

        Ok(())
    }

    /// Determine if a given node has been cached already or not.
    ///
    #[tracing::instrument(name = "Cache::is_cached", skip(node))]
    pub async fn is_cached(
        &mut self,
        node: &ComputedTarget,
    ) -> Result<CacheHitType, anyhow::Error> {
        let cache_key = self.cache_key(&node);

        match self.local_cache.is_cached(&cache_key).await? {
            CacheHitType::Miss(_) => {
                let expected_path = self.local_cache.absolute_path_for_key(&cache_key).await?;
                let _ = self
                    .remote_cache
                    .try_fetch(&cache_key, &expected_path)
                    .await;
                self.local_cache.is_cached(&cache_key).await
            }
            result => Ok(result),
        }
    }

    #[tracing::instrument(name = "Cache::evict", skip(node))]
    pub async fn evict(&mut self, node: &ComputedTarget) -> Result<(), anyhow::Error> {
        let cache_key = self.cache_key(&node);
        self.local_cache.evict(&cache_key).await
    }

    #[tracing::instrument(name = "Cache::absolute_path_by_dep")]
    pub async fn absolute_path_by_dep(&self, dep: &Dependency) -> Result<PathBuf, anyhow::Error> {
        let cache_key = self.cache_key_for_dep(dep);
        self.local_cache.absolute_path_for_key(&cache_key).await
    }

    #[tracing::instrument(name = "Cache::absolute_path_by_node")]
    pub async fn absolute_path_by_node(
        &self,
        node: &ComputedTarget,
    ) -> Result<PathBuf, anyhow::Error> {
        let cache_key = self.cache_key(node);
        self.local_cache.absolute_path_for_key(&cache_key).await
    }
}
