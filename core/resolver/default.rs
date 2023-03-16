use std::path::PathBuf;

use super::boot_resolver::BootstrapResolver;
use super::net_resolver::NetResolver;
use super::{FsResolver, ResolutionFlow, Resolver, ResolverError, TargetRegistry};
use crate::archive::ArchiveManager;
use crate::code::CodeDatabase;
use crate::model::{ConcreteTarget, Goal, RemoteTarget, Requirement, Signature, Target, TargetId};
use crate::store::DefaultStore;
use crate::tricorder::{SignatureGenerationFlow, Tricorder, TricorderManager};
use crate::worker::TaskResults;
use crate::workspace::WorkspaceManager;
use crate::{sync::*, Config};
use async_trait::async_trait;
use tracing::instrument;

#[derive(Clone)]
pub struct DefaultResolver<T: Tricorder + Clone> {
    boot_resolver: Arc<BootstrapResolver>,
    config: Config,
    fs_resolver: Arc<FsResolver<T>>,
    net_resolver: Arc<NetResolver<T>>,
    target_registry: Arc<TargetRegistry>,
}

impl<T: Tricorder + Clone + 'static> DefaultResolver<T> {
    pub fn new(
        config: Config,
        store: Arc<DefaultStore>,
        target_registry: Arc<TargetRegistry>,
        archive_manager: Arc<ArchiveManager>,
        workspace_manager: Arc<WorkspaceManager>,
        task_results: Arc<TaskResults>,
        code_db: Arc<CodeDatabase>,
    ) -> Result<Self, ResolverError> {
        let tricorder_manager = Arc::new(TricorderManager::new(config.clone(), store));

        let net_resolver = Arc::new(NetResolver::new(
            config.clone(),
            archive_manager,
            workspace_manager.clone(),
            tricorder_manager.clone(),
            target_registry.clone(),
            code_db.clone(),
        ));

        let fs_resolver = Arc::new(FsResolver::new(
            config.clone(),
            workspace_manager.clone(),
            tricorder_manager.clone(),
            target_registry.clone(),
            task_results,
            code_db.clone(),
        ));

        let boot_resolver = Arc::new(BootstrapResolver::new(
            config.clone(),
            workspace_manager.clone(),
            target_registry.clone(),
        ));

        Ok(Self {
            fs_resolver,
            net_resolver,
            boot_resolver,
            target_registry,
            config,
        })
    }
}

#[async_trait]
impl<T: Tricorder + Clone + 'static> Resolver for DefaultResolver<T> {
    #[instrument(name = "DefaultResolver::resolve", skip(self))]
    async fn resolve(
        &self,
        goal: Goal,
        target_id: TargetId,
        target: Arc<Target>,
    ) -> Result<ResolutionFlow, ResolverError> {
        let sig_flow = match &*target {
            Target::Alias(_) => todo!(),

            // NB: when our target is remote and we're building a rule, we will whip up a signature
            // on the spot to make sure the rule is instantiatable as a signature.
            Target::Remote(_t) if target.is_rule_target(&self.config) => {
                let sig = target.to_rule_signature(&self.config, target_id, target.clone());
                Ok(SignatureGenerationFlow::GeneratedSignatures {
                    signatures: vec![sig],
                })
            }

            Target::Remote(t) => self.net_resolver.resolve(goal, target_id, t).await,

            // NB: when we are bootstrapping, local targets may need a little help to get their
            // signatures generated (for example, if we are building a tricorder but we need the
            // tricorder to build itself).
            Target::Fs(t) if goal.is_bootstrap() => {
                self.boot_resolver.resolve(goal, target_id, t).await
            }

            Target::Fs(t) => self.fs_resolver.resolve(goal, target_id, t).await,
        }?;

        // TODO(@ostera): at this stage, we want to use the concrete target and the tricorder to
        // call the CodeManager and ask it to tree-split, so we can avoid regenerating signatures
        // if parts of the file we don't care about haven't changed.
        // get_ast

        match sig_flow {
            SignatureGenerationFlow::IgnoredTarget(target_id) => {
                Ok(ResolutionFlow::IgnoredTarget(target_id))
            }
            SignatureGenerationFlow::GeneratedSignatures { signatures }
                if !signatures.is_empty() =>
            {
                let signature = signatures.into_iter().next().unwrap();
                Ok(ResolutionFlow::Resolved { signature })
            }
            SignatureGenerationFlow::MissingRequirements { requirements } => {
                let mut deps = vec![];
                for req in requirements {
                    let target: Target = match req {
                        Requirement::File { path } => path.into(),
                        Requirement::Url {
                            url,
                            tricorder_url,
                            subpath,
                        } => {
                            let remote_target = RemoteTarget::builder()
                                .url(url)
                                .tricorder_url(tricorder_url)
                                .subpath(subpath.unwrap())
                                .build()?;
                            Target::Remote(remote_target)
                        }
                        Requirement::Symbol { .. } => unimplemented!(),
                        Requirement::Dependency { url, .. } => url.into(),
                    };
                    let target_id = self.target_registry.register_target(target);
                    deps.push(target_id)
                }
                Ok(ResolutionFlow::MissingDeps { deps })
            }
            _ => Err(ResolverError::Unknown("unimplemented".to_string())),
        }
    }
}

impl Target {
    pub fn to_rule_signature(
        &self,
        config: &Config,
        target_id: TargetId,
        target: Arc<Target>,
    ) -> Signature {
        let rule = target.url().unwrap().to_string();
        let target = ConcreteTarget::new(
            Goal::Build,
            target_id,
            target,
            PathBuf::new(),
            PathBuf::new(),
        );
        Signature::builder()
            .target(target)
            .rule(rule)
            .build()
            .unwrap()
    }

    pub fn is_rule_target(&self, config: &Config) -> bool {
        self.url()
            .map(|url| {
                let target_host = url.host_str().unwrap();
                let public_rule_store_host = config.public_rule_store_url().host_str().unwrap();

                target_host == public_rule_store_host
            })
            .unwrap_or_default()
    }
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use super::*;
    use crate::archive::{Archive, ArchiveManager};
    use crate::model::{ExecutableSpec, Signature};
    use crate::store::ArtifactManifest;
    use crate::tricorder::{Connection, TricorderError};
    use crate::workspace::Workspace;
    use assert_fs::prelude::*;
    use async_trait::async_trait;

    #[tokio::test]
    async fn fails_if_we_cannot_concretize_a_target() {
        let warp_root = assert_fs::TempDir::new().unwrap();
        // NOTE(@ostera): this line is useful for debugging the output directory when something
        // goes wrong.
        // let warp_root = warp_root.into_persistent();
        // dbg!(&warp_root.path());

        let config = Config::builder()
            .warp_root(warp_root.path().to_path_buf())
            .public_store_cdn_url(mockito::server_url().parse().unwrap())
            .public_store_metadata_url(mockito::server_url().parse().unwrap())
            .build()
            .unwrap();

        let am = ArchiveManager::new(&config).into();
        let store = DefaultStore::new(config.clone(), am).into();
        let target_registry = Arc::new(TargetRegistry::new());

        #[derive(Debug, Clone)]
        pub struct UnreachableTricorder;
        #[async_trait]
        impl Tricorder for UnreachableTricorder {
            async fn connect(_conn: Connection) -> Result<Self, TricorderError> {
                unreachable!()
            }

            async fn ensure_ready(&mut self) -> Result<(), TricorderError> {
                unreachable!()
            }

            async fn generate_signature(
                &mut self,
                _: &ConcreteTarget,
                _: &[(Arc<ExecutableSpec>, Arc<ArtifactManifest>)],
            ) -> Result<SignatureGenerationFlow, TricorderError> {
                unreachable!();
            }

            async fn ready_dependency(
                &mut self,
                _concrete_target: &ConcreteTarget,
                _archive: &Archive,
            ) -> Result<SignatureGenerationFlow, TricorderError> {
                unreachable!()
            }
        }

        let workspace_manager = WorkspaceManager::new(config.clone());
        let workspace = Workspace::default();
        let wid = workspace_manager
            .register_local_workspace(workspace)
            .unwrap();
        workspace_manager.set_current_workspace(wid);

        let archive_manager = Arc::new(ArchiveManager::new(&config));
        let task_results = Arc::new(TaskResults::new(target_registry.clone()));

        let code_db = Arc::new(CodeDatabase::new(config.clone()).unwrap());
        let r: DefaultResolver<UnreachableTricorder> = DefaultResolver::new(
            config,
            store,
            target_registry.clone(),
            archive_manager.clone(),
            workspace_manager.into(),
            task_results,
            code_db,
        )
        .unwrap();

        let target: Target = "bad/file/path.ex".into();
        let target_id = target_registry.register_target(&target);
        let result = r.resolve(Goal::Build, target_id, target.into()).await;

        assert_matches!(
            result.unwrap_err(),
        ResolverError::CouldNotFindFile { path } if path == PathBuf::from("bad/file/path.ex")
        );
    }

    #[tokio::test]
    async fn resolves_a_signature_for_a_target() {
        let warp_root = assert_fs::TempDir::new().unwrap();
        // NOTE(@ostera): this line is useful for debugging the output directory when something goes wrong.
        // let warp_root = warp_root.into_persistent();
        // dbg!(&warp_root.path());

        let curr_workspace = assert_fs::TempDir::new().unwrap();
        // NOTE(@ostera): this line is useful for debugging the output directory when something goes wrong.
        // let curr_workspace = curr_workspace.into_persistent();
        // dbg!(&curr_workspace.path());
        let file_target = curr_workspace.child("good_file.ex");
        file_target.write_str("dummy data").unwrap();

        let config = Config::builder()
            .warp_root(warp_root.path().to_path_buf())
            .invocation_dir(curr_workspace.path().to_path_buf())
            .public_store_cdn_url(mockito::server_url().parse().unwrap())
            .public_store_metadata_url(mockito::server_url().parse().unwrap())
            .build()
            .unwrap();

        let am = ArchiveManager::new(&config).into();
        let store = DefaultStore::new(config.clone(), am).into();
        let target_registry = Arc::new(TargetRegistry::new());

        #[derive(Debug, Clone)]
        pub struct HappyPathTricorder;
        #[async_trait]
        impl Tricorder for HappyPathTricorder {
            async fn connect(_conn: Connection) -> Result<Self, TricorderError> {
                Ok(Self)
            }

            async fn ensure_ready(&mut self) -> Result<(), TricorderError> {
                Ok(())
            }

            async fn generate_signature(
                &mut self,
                concrete_target: &ConcreteTarget,
                _: &[(Arc<ExecutableSpec>, Arc<ArtifactManifest>)],
            ) -> Result<SignatureGenerationFlow, TricorderError> {
                Ok(SignatureGenerationFlow::GeneratedSignatures {
                    signatures: vec![Signature::builder()
                        .target((*concrete_target).clone())
                        .rule("test_rule".to_string())
                        .build()
                        .unwrap()],
                })
            }

            async fn ready_dependency(
                &mut self,
                _concrete_target: &ConcreteTarget,
                _archive: &Archive,
            ) -> Result<SignatureGenerationFlow, TricorderError> {
                unreachable!()
            }
        }
        let workspace_manager = WorkspaceManager::new(config.clone());
        let workspace = Workspace::default();
        let wid = workspace_manager
            .register_local_workspace(workspace)
            .unwrap();

        let archive_manager = Arc::new(ArchiveManager::new(&config));
        let task_results = Arc::new(TaskResults::new(target_registry.clone()));

        workspace_manager.set_current_workspace(wid);
        let code_db = Arc::new(CodeDatabase::new(config.clone()).unwrap());
        let r: DefaultResolver<HappyPathTricorder> = DefaultResolver::new(
            config,
            store,
            target_registry.clone(),
            archive_manager.clone(),
            workspace_manager.into(),
            task_results,
            code_db,
        )
        .unwrap();

        // NOTE(@ostera): this mock will be used to not fetch the real tricorder
        let _public_store_mock1 = mockito::mock("GET", "/a-hash.tar.gz")
            .with_status(200)
            .with_body(include_bytes!("./fixtures/sample_artifact.tar.gz"))
            .create();

        // NOTE(@ostera): this mock will be used to download the manifest
        let _package_manifest_mock = mockito::mock("GET", "/tricorder/beam/manifest.json")
            .with_status(200)
            .with_body(
                r#"
{
    "published_at": "2023-03-01T21:09:32+00:00",
    "keys": {
        "aarch64-apple-darwin": [ "a-hash" ],
        "x86_64-apple-darwin": [ "a-hash" ],
        "aarch64-unknown-linux-gnu": [ "a-hash" ],
        "x86_64-unknown-linux-gnu": [ "a-hash" ]
    }
}
                "#,
            )
            .create();

        let target: Target = curr_workspace.path().join("good_file.ex").into();
        let target_id = target_registry.register_target(&target);
        let resolution = r
            .resolve(Goal::Build, target_id, target.into())
            .await
            .unwrap();

        assert_matches!(
            resolution,
            ResolutionFlow::Resolved { signature } if signature.target().path().file_name().unwrap().to_str().unwrap() == "good_file.ex" && signature.rule() == "test_rule"
        );
    }
}
