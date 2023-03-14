use std::path::PathBuf;

use super::{FsResolver, ResolutionFlow, Resolver, ResolverError, TargetRegistry};
use crate::model::{rule, ConcreteTarget, Goal, Requirement, Signature, Target, TargetId};
use crate::store::DefaultStore;
use crate::tricorder::{SignatureGenerationFlow, Tricorder, TricorderManager};
use crate::workspace::WorkspaceManager;
use crate::{sync::*, Config};
use async_trait::async_trait;
use tracing::instrument;

#[derive(Clone)]
pub struct DefaultResolver<T: Tricorder + Clone> {
    fs_resolver: Arc<FsResolver>,
    tricorder_manager: Arc<TricorderManager<T, DefaultStore>>,
    target_registry: Arc<TargetRegistry>,
    workspace_manager: Arc<WorkspaceManager>,
    config: Config,
}

impl<T: Tricorder + Clone + 'static> DefaultResolver<T> {
    pub fn new(
        config: Config,
        store: Arc<DefaultStore>,
        target_registry: Arc<TargetRegistry>,
        workspace_manager: Arc<WorkspaceManager>,
    ) -> Self {
        let fs_resolver = Arc::new(FsResolver::new());
        let tricorder_manager = Arc::new(TricorderManager::new(config.clone(), store));
        Self {
            fs_resolver,
            tricorder_manager,
            target_registry,
            workspace_manager,
            config,
        }
    }

    async fn concretize_target(
        &self,
        goal: Goal,
        target_id: TargetId,
        target: Arc<Target>,
    ) -> Result<Arc<ConcreteTarget>, ResolverError> {
        let workspace = self.workspace_manager.current_workspace();

        let final_path = match &*target {
            // Target::Alias(a) => self.alias_resolver.resolve(goal, a).await?,
            // Target::Remote(r) => self.net_resolver.resolve(goal, r).await?,
            Target::Fs(f) => self.fs_resolver.resolve(goal, f).await?,
            _ => todo!(),
        };

        let final_path = if final_path.starts_with(workspace.root()) {
            final_path
                .strip_prefix(workspace.root())
                .unwrap()
                .to_path_buf()
        } else {
            final_path
        };

        let ct = ConcreteTarget::new(
            goal,
            target_id,
            target,
            final_path,
            workspace.root().to_path_buf(),
        );

        Ok(self
            .target_registry
            .associate_concrete_target(target_id, ct))
    }

    fn bootstrap_signature(
        &self,
        concrete_target: Arc<ConcreteTarget>,
    ) -> Result<ResolutionFlow, ResolverError> {
        if let Some(signature) = SignatureBootstrapper::for_concrete_target(&concrete_target) {
            Ok(ResolutionFlow::Resolved { signature })
        } else {
            Ok(ResolutionFlow::IgnoredTarget(concrete_target.target_id()))
        }
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
        if let Some(signature) =
            ToolchainSignatures.for_target(&self.config, target_id, target.clone())
        {
            return Ok(ResolutionFlow::Resolved { signature });
        }

        let concrete_target = self.concretize_target(goal, target_id, target).await?;

        if let Goal::Bootstrap = goal {
            return self.bootstrap_signature(concrete_target);
        }

        // 1. find and ready the tricorder
        let mut tricorder = if let Some(tricorder) = self
            .tricorder_manager
            .find_and_ready(&concrete_target)
            .await?
        {
            tricorder
        } else {
            return Ok(ResolutionFlow::IgnoredTarget(target_id));
        };

        // TODO(@ostera): at this stage, we want to use the concrete target and the tricorder to
        // call the CodeManager and ask it to tree-split, so we can avoid regenerating signatures
        // if parts of the file we don't care about haven't changed.
        // get_ast

        // 2. generate signature for this concrete target
        let sig_flow = tricorder.generate_signature(&concrete_target).await?;

        match sig_flow {
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
                        Requirement::Url { url } => url.into(),
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

struct SignatureBootstrapper;

impl SignatureBootstrapper {
    pub fn for_concrete_target(concrete_target: &ConcreteTarget) -> Option<Signature> {
        match concrete_target.name() {
            "mix.exs" => Some(Self::mix_escript(concrete_target.clone())),
            "Cargo.toml" => Some(Self::cargo_binary(concrete_target.clone())),
            _ => None,
        }
    }

    pub fn cargo_binary(concrete_target: ConcreteTarget) -> Signature {
        Signature::builder()
            .rule("cargo_binary".to_string())
            .config({
                let mut config = rule::Config::default();
                config.insert("name".to_string(), concrete_target.name().into());
                config.insert("bin".to_string(), "tricorder".into());
                config
            })
            .target(concrete_target)
            .build()
            .unwrap()
    }

    pub fn mix_escript(concrete_target: ConcreteTarget) -> Signature {
        Signature::builder()
            .rule("mix_escript".to_string())
            .config({
                let mut config = rule::Config::default();
                config.insert("name".to_string(), concrete_target.name().into());
                config.insert("bin".to_string(), "tricorder".into());
                config
            })
            .target(concrete_target)
            .build()
            .unwrap()
    }
}

struct ToolchainSignatures;

impl ToolchainSignatures {
    pub fn for_target(
        &self,
        config: &Config,
        target_id: TargetId,
        target: Arc<Target>,
    ) -> Option<Signature> {
        if self.is_target_a_rule(config, &target) {
            return Some(self.rule_target_signature(target_id, target));
        }
        None
    }

    fn rule_target_signature(&self, target_id: TargetId, target: Arc<Target>) -> Signature {
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

    fn is_target_a_rule(&self, config: &Config, target: &Target) -> bool {
        target
            .url()
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
    use crate::archive::ArchiveManager;
    use crate::model::Signature;
    use crate::resolver::fs_resolver::FsResolverError;
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
            ) -> Result<SignatureGenerationFlow, TricorderError> {
                unreachable!();
            }
        }

        let workspace_manager = WorkspaceManager::new();
        let workspace = Workspace::default();
        let wid = workspace_manager
            .register_local_workspace(workspace)
            .unwrap();
        workspace_manager.set_current_workspace(wid);

        let r: DefaultResolver<UnreachableTricorder> = DefaultResolver::new(
            config,
            store,
            target_registry.clone(),
            workspace_manager.into(),
        );

        let target: Target = "bad/file/path.ex".into();
        let target_id = target_registry.register_target(&target);
        let result = r.resolve(Goal::Build, target_id, target.into()).await;

        assert_matches!(
            result.unwrap_err(),
        ResolverError::FsResolverError(FsResolverError::CouldNotFindFile { path }) if path == PathBuf::from("bad/file/path.ex")
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
            ) -> Result<SignatureGenerationFlow, TricorderError> {
                Ok(SignatureGenerationFlow::GeneratedSignatures {
                    signatures: vec![Signature::builder()
                        .target((*concrete_target).clone())
                        .rule("test_rule".to_string())
                        .build()
                        .unwrap()],
                })
            }
        }
        let workspace_manager = WorkspaceManager::new();
        let workspace = Workspace::default();
        let wid = workspace_manager
            .register_local_workspace(workspace)
            .unwrap();

        workspace_manager.set_current_workspace(wid);
        let r: DefaultResolver<HappyPathTricorder> = DefaultResolver::new(
            config,
            store,
            target_registry.clone(),
            workspace_manager.into(),
        );

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
