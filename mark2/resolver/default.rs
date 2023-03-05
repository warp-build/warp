use super::{FsResolver, ResolutionFlow, Resolver, ResolverError};
use crate::model::{ConcreteTarget, Goal, Target};
use crate::store::DefaultStore;
use crate::tricorder::{SignatureGenerationFlow, Tricorder, TricorderManager};
use crate::{sync::*, Config};
use async_trait::async_trait;

#[derive(Clone)]
pub struct DefaultResolver<T: Tricorder + Clone> {
    fs_resolver: Arc<FsResolver>,
    tricorder_manager: Arc<TricorderManager<T, DefaultStore>>,
}

impl<T: Tricorder + Clone + 'static> DefaultResolver<T> {
    pub fn new(config: Config, store: Arc<DefaultStore>) -> Self {
        let fs_resolver = Arc::new(FsResolver::new());
        let tricorder_manager = Arc::new(TricorderManager::new(config, store));
        Self {
            fs_resolver,
            tricorder_manager,
        }
    }

    async fn concretize_target(
        &self,
        goal: Goal,
        target: Arc<Target>,
    ) -> Result<ConcreteTarget, ResolverError> {
        let final_path = match &*target {
            // Target::Alias(a) => self.alias_resolver.resolve(goal, a).await?,
            // Target::Remote(r) => self.net_resolver.resolve(goal, r).await?,
            Target::Fs(f) => self.fs_resolver.resolve(goal, f).await?,
            _ => todo!(),
        };
        Ok(ConcreteTarget::new(goal, target, final_path))
    }
}

#[async_trait]
impl<T: Tricorder + Clone + 'static> Resolver for DefaultResolver<T> {
    async fn resolve(
        &self,
        goal: Goal,
        target: Arc<Target>,
    ) -> Result<ResolutionFlow, ResolverError> {
        let concrete_target = self.concretize_target(goal, target).await?;

        // 1. find and ready the tricorder
        let tricorder = self
            .tricorder_manager
            .find_and_ready(&concrete_target)
            .await?;

        // TODO(@ostera): at this stage, we want to use the concrete target and the tricorder to
        // call the CodeManager and ask it to tree-split, so we can avoid regenerating signatures
        // if parts of the file we don't care about haven't changed.

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
                Ok(ResolutionFlow::MissingDependencies { requirements })
            }
            _ => todo!(),
        }
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

        #[derive(Debug, Clone)]
        pub struct UnreachableTricorder;
        #[async_trait]
        impl Tricorder for UnreachableTricorder {
            async fn connect(_conn: Connection) -> Result<Self, TricorderError> {
                unreachable!()
            }

            async fn ensure_ready(&self) -> Result<(), TricorderError> {
                unreachable!()
            }

            async fn generate_signature(
                &self,
                _: &ConcreteTarget,
            ) -> Result<SignatureGenerationFlow, TricorderError> {
                unreachable!();
            }
        }

        let r: DefaultResolver<UnreachableTricorder> = DefaultResolver::new(config, store);

        let target: Target = "bad/file/path.ex".into();
        let result = r.resolve(Goal::Build, target.into()).await;

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

        #[derive(Debug, Clone)]
        pub struct HappyPathTricorder;
        #[async_trait]
        impl Tricorder for HappyPathTricorder {
            async fn connect(_conn: Connection) -> Result<Self, TricorderError> {
                Ok(Self)
            }

            async fn ensure_ready(&self) -> Result<(), TricorderError> {
                Ok(())
            }

            async fn generate_signature(
                &self,
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
        let r: DefaultResolver<HappyPathTricorder> = DefaultResolver::new(config, store);

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
        let resolution = r.resolve(Goal::Build, target.into()).await.unwrap();

        assert_matches!(
            resolution,
            ResolutionFlow::Resolved { signature } if signature.target().path().file_name().unwrap().to_str().unwrap() == "good_file.ex" && signature.rule() == "test_rule"
        );
    }
}
