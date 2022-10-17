use super::Event;
use super::*;
use dashmap::DashMap;
use std::{path::PathBuf, sync::Arc};
use thiserror::*;
use tracing::*;

#[derive(Error, Debug)]
pub enum DependencyResolverError {
    #[error("Can't resolve a dependency with a URL that has no host: {0:?}")]
    UrlHadNoHost(url::Url),

    #[error(transparent)]
    ArchiveManagerError(ArchiveManagerError),

    #[error(transparent)]
    CommandRunnerError(CommandRunnerError),

    #[error("We don't yet have resolver {} built!", .resolver.to_string())]
    MissingResolver { resolver: Label },
}

#[derive(Debug)]
pub struct DependencyResolver {
    build_results: Arc<BuildResults>,
    targets: DashMap<Label, Target>,
    resolvers: DashMap<String, Label>,
    event_channel: Arc<EventChannel>,
}

impl DependencyResolver {
    #[tracing::instrument(name = "DependencyResolver::new", skip(workspace))]
    pub fn new(
        workspace: &Workspace,
        build_results: Arc<BuildResults>,
        event_channel: Arc<EventChannel>,
    ) -> Self {
        let this = Self {
            build_results,
            targets: DashMap::new(),
            resolvers: DashMap::new(),
            event_channel,
        };

        // TODO(@ostera): these should come from a registry, like the toolchains registry
        for (host, resolver) in [
            ("hex.pm", "https://tools.warp.build/hexpm/resolver"),
            ("github.com", "https://tools.warp.build/github/resolver"),
            ("npmjs.com", "https://tools.warp.build/npm/resolver"),
        ] {
            this.resolvers
                .insert(host.to_string(), Label::new(resolver));
        }

        this
    }

    #[tracing::instrument(name = "DependencyResolver::get", skip(self))]
    pub async fn get(&self, label: &Label) -> Result<Option<Target>, DependencyResolverError> {
        if let Some(entry) = self.targets.get(label) {
            let target = entry.value().clone();
            return Ok(Some(target));
        }

        let host = label.url().host().unwrap().to_string();
        if let Some(ref resolver) = self.resolvers.get(&host) {
            if let Some(target) = self.resolve(resolver, label).await? {
                self.targets.insert(label.clone(), target.clone());
                return Ok(Some(target));
            }
        }

        Ok(None)
    }

    #[tracing::instrument(name = "DependencyResolver::resolve", skip(self))]
    async fn resolve(
        &self,
        resolver: &Label,
        label: &Label,
    ) -> Result<Option<Target>, DependencyResolverError> {
        // 1. build resolver
        let (manifest, target) = self
            .build_results
            .get_computed_target(resolver)
            .ok_or_else(|| DependencyResolverError::MissingResolver {
                resolver: resolver.clone(),
            })?;

        self.event_channel.send(Event::ResolvingDependency {
            label: label.clone(),
        });

        // 2. run resolver passing in the label url as a parameter
        let cmd = CommandRunner::builder()
            .cwd(PathBuf::from("."))
            .manifest(manifest)
            .target(target)
            .args(vec!["resolve".to_string(), label.to_string()])
            .sandboxed(true)
            .stream_outputs(true)
            .build()
            .map_err(DependencyResolverError::CommandRunnerError)?;

        let results = cmd
            .run()
            .await
            .map_err(DependencyResolverError::CommandRunnerError)?;

        // 3. parse results into a Target
        if results.is_empty() {
            Ok(None)
        } else {
            // let target: Target = serde_json::from_slice(results.as_bytes())
            //     .map_err(DependencyResolverError::ParseError)?;
            Ok(None)
        }
    }
}
