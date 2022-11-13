use super::Event;
use super::*;
use dashmap::DashMap;
use std::{path::PathBuf, sync::Arc};
use thiserror::*;
use tokio::fs;
use tokio::io::AsyncReadExt;
use tracing::*;
use url::Url;

/// A Label Resolver that handles turning references to external dependencies (such as
/// `https://hex.pm/packages/proper` and `https://github.com/warp-build/core`) into buildable
/// targets.
///
#[derive(Debug, Clone)]
pub struct DependencyResolver {
    archive_manager: ArchiveManager,
    artifact_store: Arc<ArtifactStore>,
    build_results: Arc<BuildResults>,
    dependency_manager: Arc<DependencyManager>,
    event_channel: Arc<EventChannel>,
    global_workspaces_path: PathBuf,
    label_registry: Arc<LabelRegistry>,
    resolver_service_manager: Arc<ResolverServiceManager>,
    resolvers: DashMap<String, LabelId>,
    targets: DashMap<LabelId, Target>,
    workspace: Workspace,
}

#[derive(Error, Debug)]
pub enum DependencyResolverError {
    #[error("Can't resolve a dependency with a URL that has no host: {0:?}")]
    UrlHadNoHost(url::Url),

    #[error(transparent)]
    ArchiveManagerError(ArchiveManagerError),

    #[error(transparent)]
    CommandRunnerError(CommandRunnerError),

    #[error("We don't yet have resolver built!")]
    MissingResolver { resolver: LabelId },

    #[error("Dependency {} has no version in the Workspace.json", .dependency.to_string())]
    UnspecifiedDependency { dependency: Label },

    #[error("Could not parse Dependency Resolution contents: \n\n{resolution}\n\ndue to {err:#?}")]
    ParseError {
        err: serde_json::Error,
        resolution: String,
    },

    #[error(transparent)]
    ExtractionError(ArchiveManagerError),

    #[error(transparent)]
    ResolverServiceManagerError(ResolverServiceManagerError),

    #[error(transparent)]
    ResolverServiceError(tonic::Status),

    #[error(transparent)]
    ArchiveError(ArchiveError),

    #[error(transparent)]
    CodeDbError(CodeDbError),
}

impl DependencyResolver {
    #[tracing::instrument(name = "DependencyResolver::new", skip(workspace))]
    pub fn new(
        workspace: &Workspace,
        build_results: Arc<BuildResults>,
        event_channel: Arc<EventChannel>,
        artifact_store: Arc<ArtifactStore>,
        label_registry: Arc<LabelRegistry>,
        dependency_manager: Arc<DependencyManager>,
        resolver_service_manager: Arc<ResolverServiceManager>,
    ) -> Self {
        let this = Self {
            archive_manager: ArchiveManager::new(workspace, event_channel.clone()),
            artifact_store,
            build_results,
            dependency_manager,
            event_channel,
            global_workspaces_path: workspace.paths.global_workspaces_path.clone(),
            label_registry,
            resolver_service_manager,
            resolvers: DashMap::new(),
            targets: DashMap::new(),
            workspace: workspace.to_owned(),
        };

        // TODO(@ostera): these should come from a registry, like the toolchains registry
        for (host, resolver) in [
            ("hex.pm", "https://tools.warp.build/hexpm/resolver"),
            ("github.com", "https://tools.warp.build/github/resolver"),
            ("gitlab.com", "https://tools.warp.build/gitlab/resolver"),
            ("npmjs.com", "https://tools.warp.build/npm/resolver"),
        ] {
            let label: Label = Url::parse(resolver).unwrap().into();
            let label_id = this.label_registry.register_label(label);
            this.resolvers.insert(host.to_string(), label_id);
        }

        this
    }

    #[tracing::instrument(name = "DependencyResolver::resolve", skip(self))]
    pub async fn resolve(
        &self,
        label_id: LabelId,
        label: &Label,
    ) -> Result<Option<Target>, LabelResolverError> {
        if let Some(entry) = self.targets.get(&label_id) {
            let target = entry.value().clone();
            return Ok(Some(target));
        }

        let remote_label = label.get_remote().unwrap();

        let dependency = self
            .dependency_manager
            .get(label_id)
            .ok_or_else(|| DependencyResolverError::UnspecifiedDependency {
                dependency: remote_label.to_owned().into(),
            })
            .map_err(LabelResolverError::DependencyResolverError)?;

        if let Some(resolver) = self.resolvers.get(&remote_label.host) {
            if let Some(target) = self
                .do_resolve(*resolver, label_id, remote_label, dependency)
                .await
                .map_err(LabelResolverError::DependencyResolverError)?
            {
                self.targets.insert(label_id, target.clone());
                return Ok(Some(target));
            }
        }

        Ok(None)
    }

    async fn do_resolve(
        &self,
        resolver: LabelId,
        label: LabelId,
        remote_label: &RemoteLabel,
        dependency: Dependency,
    ) -> Result<Option<Target>, DependencyResolverError> {
        // 2. Resolve dependency

        // 2.1. create a workspace for this dependency

        // https://hex.pm/packages/proper -> become a workspace
        // let mut dep_workspace = DependencyWorkspace::from_label(label);
        let url = remote_label.url_str().replace("://", "/");

        // WorkspaceManager.prepare_workspace(&mut dep_workspace).await?;
        let final_dir = self
            .global_workspaces_path
            .join(url)
            .join(&dependency.version);
        fs::create_dir_all(&final_dir).await.unwrap();

        let workspace_name = {
            use sha2::{Digest, Sha256};
            let mut hasher = Sha256::new();
            hasher.update(final_dir.to_str().unwrap());
            format!(
                "{:x}-{}",
                hasher.finalize(),
                final_dir.file_name().unwrap().to_str().unwrap(),
            )
        };

        self.artifact_store
            .register_workspace_raw(final_dir.clone(), PathBuf::from(workspace_name));

        // -- interlude --
        // try to fetch the resolution file from disk
        let signature_file_path = final_dir.join("Warp.signature");

        let resolution = if fs::metadata(&signature_file_path).await.is_ok() {
            let mut signature_file = fs::File::open(signature_file_path).await.unwrap();
            let mut bytes = vec![];
            signature_file.read_to_end(&mut bytes).await.unwrap();
            serde_json::from_slice(&bytes).unwrap()
        } else {
            self.resolve_and_download(remote_label, resolver, &final_dir, &dependency)
                .await?;

            self.generate_signature(resolver, &final_dir, &dependency)
                .await?
        };

        // 4. Turn our new signature into a Target
        let sig = resolution.signatures[0].clone();
        let mut target: Target = sig.into();

        // 5. The Target itself will have a label that is not pointing to the right workspace
        //    (since it will be considered "local" to itself, but we are in a different workspace).
        //
        //    To fix this, we will turn it into a LocalLabel and set the workspace to the current
        //    dependency workspace.
        //
        target.label = target.label.to_local(&final_dir).unwrap();
        target.label.set_associated_url(remote_label.url());

        Ok(Some(target))
    }

    async fn resolve_and_download(
        &self,
        remote_label: &RemoteLabel,
        resolver: LabelId,
        final_dir: &PathBuf,
        dependency: &Dependency,
    ) -> Result<(), DependencyResolverError> {
        let mut main_resolver_svc = self
            .resolver_service_manager
            .start(resolver)
            .await
            .map_err(DependencyResolverError::ResolverServiceManagerError)?;

        self.event_channel.send(Event::ResolvingDependency {
            label: remote_label.to_owned().into(),
            resolver: self.label_registry.get_label(resolver).as_ref().to_owned(),
        });

        let request = proto::build::warp::dependency::ResolveDependencyRequest {
            package_name: dependency.package.clone(),
            version: dependency.version.clone(),
            url: dependency.url.to_string(),
        };

        let response = main_resolver_svc
            .resolve_dependency(request)
            .await
            .map_err(DependencyResolverError::ResolverServiceError)?
            .into_inner();

        let archive: Archive = response
            .archive
            .unwrap()
            .try_into()
            .map_err(DependencyResolverError::ArchiveError)?;

        self.archive_manager
            .download_and_extract(&archive.url, &final_dir, None, archive.strip_prefix)
            .await
            .map_err(DependencyResolverError::ExtractionError)?;

        Ok(())
    }

    async fn generate_signature(
        &self,
        resolver: LabelId,
        final_dir: &PathBuf,
        dependency: &Dependency,
    ) -> Result<GeneratedSignature, DependencyResolverError> {
        let override_resolver = dependency.resolver.unwrap_or_else(|| resolver);
        let mut override_resolver_svc = self
            .resolver_service_manager
            .start(override_resolver)
            .await
            .map_err(DependencyResolverError::ResolverServiceManagerError)?;

        let request = proto::build::warp::dependency::PrepareDependencyRequest {
            package_root: final_dir.to_string_lossy().to_string(),
            url: dependency.url.to_string(),
            version: dependency.version.clone(),
            package_name: dependency.package.clone(),
        };

        let response = override_resolver_svc
            .prepare_dependency(request)
            .await
            .map_err(DependencyResolverError::ResolverServiceError)?
            .into_inner();

        let code_db = CodeDb::new(&self.workspace).await.unwrap();

        let mut signatures = vec![];
        for sig in response.signatures {
            let mut deps = vec![];
            for dep in sig.deps {
                let req = dep.requirement.unwrap();
                match req {
                    proto::build::warp::requirement::Requirement::Url(url_req) => {
                        let url: url::Url = url_req.url.parse().unwrap();
                        let label = url.into();
                        deps.push(label)
                    }
                    proto::build::warp::requirement::Requirement::Dependency(dep_req) => {
                        /*
                        let label = code_db.find_label_for_file(&file_req.path).unwrap();
                        deps.push(label)
                        */
                    }
                    proto::build::warp::requirement::Requirement::File(file_req) => {
                        /*
                        let label = code_db.find_label_for_file(&file_req.path).unwrap();
                        deps.push(label)
                        */
                    }
                    proto::build::warp::requirement::Requirement::Symbol(sym_req)
                        if sym_req.kind == "module" =>
                    {
                        if let Ok(label) = code_db
                            .find_label_for_symbol(&sym_req.raw, &sym_req.kind)
                            .await
                            .map_err(DependencyResolverError::CodeDbError)
                        {
                            deps.push(label)
                        } else {
                            let url: Url = format!("https://hex.pm/packages/{}", sym_req.raw)
                                .parse()
                                .unwrap();
                            let label: Label = url.into();
                            deps.push(label);
                        }
                    }
                    proto::build::warp::requirement::Requirement::Symbol(sym_req) => {
                        let label = code_db
                            .find_label_for_symbol(&sym_req.raw, &sym_req.kind)
                            .await
                            .map_err(DependencyResolverError::CodeDbError)?;
                        deps.push(label)
                    }
                }
            }

            let mut runtime_deps = vec![];
            for dep in sig.runtime_deps {
                let req = dep.requirement.unwrap();
                match req {
                    proto::build::warp::requirement::Requirement::Url(url_req) => {
                        let url: url::Url = url_req.url.parse().unwrap();
                        let label = url.into();
                        deps.push(label)
                    }
                    proto::build::warp::requirement::Requirement::Dependency(dep_req) => {
                        /*
                        let label = code_db.find_label_for_file(&file_req.path).unwrap();
                        deps.push(label)
                        */
                    }
                    proto::build::warp::requirement::Requirement::File(file_req) => {
                        /*
                        let label = code_db.find_label_for_file(&file_req.path).unwrap();
                        deps.push(label)
                        */
                    }
                    proto::build::warp::requirement::Requirement::Symbol(sym_req)
                        if sym_req.kind == "module" =>
                    {
                        if let Ok(label) = code_db
                            .find_label_for_symbol(&sym_req.raw, &sym_req.kind)
                            .await
                            .map_err(DependencyResolverError::CodeDbError)
                        {
                            runtime_deps.push(label)
                        } else {
                            let url: Url = format!("https://hex.pm/packages/{}", sym_req.raw)
                                .parse()
                                .unwrap();
                            let label: Label = url.into();
                            runtime_deps.push(label);
                        }
                    }
                    proto::build::warp::requirement::Requirement::Symbol(sym_req) => {
                        let label = code_db
                            .find_label_for_symbol(&sym_req.raw, &sym_req.kind)
                            .await
                            .map_err(DependencyResolverError::CodeDbError)?;
                        runtime_deps.push(label)
                    }
                }
            }

            let signature = Signature {
                name: sig.name.parse().unwrap(),
                rule: sig.rule,
                deps,
                runtime_deps,
                config: sig.config.map(|c| c.into()).unwrap_or_default(),
            };

            signatures.push(signature)
        }

        let gen_sig = GeneratedSignature { signatures };

        let json = serde_json::to_string_pretty(&gen_sig).unwrap();
        fs::write(final_dir.join("Warp.signature"), json)
            .await
            .unwrap();

        Ok(gen_sig)
    }
}
