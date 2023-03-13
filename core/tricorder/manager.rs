use super::{
    Connection, Tricorder, TricorderError, TricorderRegistry, TricorderRegistryError,
    DEFAULT_TRICODER_BINARY_NAME,
};
use crate::model::ConcreteTarget;
use crate::store::{ArtifactManifest, ManifestUrl, Store, StoreError};
use crate::sync::*;
use crate::util::port_finder::PortFinder;
use crate::util::process_pool::{ProcessId, ProcessPool, ProcessPoolError, ProcessSpec};
use crate::Config;
use dashmap::DashMap;
use std::collections::HashMap;
use std::marker::PhantomData;
use thiserror::*;
use tracing::instrument;

/// A manager of Tricorder processes and creator of clients. This struct keeps a thread-safe pool
/// of processes that can be used to create new clients for existing tricorders whenever needed.
///
pub struct TricorderManager<T: Tricorder, S: Store> {
    registry: TricorderRegistry,
    process_pool: ProcessPool<T>,
    artifact_store: Arc<S>,
    tricorders: DashMap<ManifestUrl, (ProcessId<T>, Connection)>,

    // NOTE(@ostera): only used to serialize the calls to `next` and prevent fetching the same
    // target twice.
    _lock: Arc<tokio::sync::Mutex<()>>,
}

impl<T, S> TricorderManager<T, S>
where
    T: Tricorder + 'static,
    S: Store,
{
    pub fn new(config: Config, artifact_store: Arc<S>) -> Self {
        Self {
            registry: TricorderRegistry::new(config),
            process_pool: ProcessPool::new(),
            artifact_store,
            tricorders: Default::default(),
            _lock: Arc::new(tokio::sync::Mutex::new(())),
        }
    }

    #[instrument(name = "TricorderManager::find_and_ready", skip(self, concrete_target))]
    pub async fn find_and_ready(
        &self,
        concrete_target: &ConcreteTarget,
    ) -> Result<Option<impl Tricorder>, TricorderManagerError> {
        let _lock = self._lock.lock().await;

        // 1. find exactly which tricorder we need.
        //    if we can't find one, that's also okay, we'll just skip this target.
        let tricorder_url = if let Some(tricorder_url) =
            self.registry.find_by_path(concrete_target.path()).await?
        {
            tricorder_url
        } else {
            return Ok(None);
        };

        if let Some(entry) = self.tricorders.get(&tricorder_url) {
            let (_pid, conn) = &*entry;
            let tricorder = T::connect(*conn).await?;
            return Ok(Some(tricorder));
        }

        // 2. install it
        let artifact_manifest = self
            .artifact_store
            .install_from_manifest_url(&tricorder_url)
            .await?;

        // 3. start it
        let port = PortFinder::next().unwrap();
        let bin = self
            .artifact_store
            .canonicalize_provided_artifact(&artifact_manifest, DEFAULT_TRICODER_BINARY_NAME)
            .ok_or_else(|| TricorderManagerError::CouldNotFindTricoderBinary {
                artifact_manifest: artifact_manifest.clone(),
            })?;

        let shell_env: HashMap<String, String> = artifact_manifest
            .shell_env()
            .iter()
            .map(|(k, v)| (k.to_string(), v.to_string()))
            .collect();

        let spec = ProcessSpec {
            bin: bin.to_path_buf(),
            args: vec!["start".to_string(), port.to_string()],
            env: shell_env,
            current_dir: None,
            _process_type: PhantomData,
        };
        let pid = self.process_pool.spawn(spec).await?;

        // 4. connect to it
        let conn = Connection { port };
        let mut tricorder = T::connect(conn).await?;

        // 5. ready it
        tricorder.ensure_ready().await?;

        self.tricorders.insert(tricorder_url, (pid, conn));

        Ok(Some(tricorder))
    }
}

#[derive(Error, Debug)]
pub enum TricorderManagerError {
    #[error(transparent)]
    TricorderRegistryError(TricorderRegistryError),

    #[error(transparent)]
    TricorderError(TricorderError),

    #[error(transparent)]
    StoreError(StoreError),

    #[error(transparent)]
    ProcessPoolError(ProcessPoolError),

    #[error("Could not find a provided 'tricorder' binary in the artifact {}", .artifact_manifest.target())]
    CouldNotFindTricoderBinary { artifact_manifest: ArtifactManifest },
}

impl From<TricorderError> for TricorderManagerError {
    fn from(err: TricorderError) -> Self {
        Self::TricorderError(err)
    }
}

impl From<TricorderRegistryError> for TricorderManagerError {
    fn from(err: TricorderRegistryError) -> Self {
        Self::TricorderRegistryError(err)
    }
}

impl From<ProcessPoolError> for TricorderManagerError {
    fn from(value: ProcessPoolError) -> Self {
        Self::ProcessPoolError(value)
    }
}

impl From<StoreError> for TricorderManagerError {
    fn from(value: StoreError) -> Self {
        Self::StoreError(value)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::archive::ArchiveManager;
    use crate::model::{Goal, Target};
    use crate::resolver::TargetRegistry;
    use crate::store::DefaultStore;
    use crate::tricorder::{Connection, SignatureGenerationFlow};
    use crate::Config;
    use assert_fs::prelude::*;
    use async_trait::async_trait;
    use std::path::PathBuf;

    #[tokio::test]
    async fn installs_a_tricorder_from_a_concrete_target() {
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

        // NOTE(@ostera): this mock will be used by the public store to fetch an actual artifact
        let _public_store_mock1 = mockito::mock("GET", "/a-hash.tar.gz")
            .with_status(200)
            .with_body(include_bytes!("./fixtures/sample_artifact.tar.gz"))
            .create();

        let _public_store_mock2 = mockito::mock("GET", "/b-hash.tar.gz")
            .with_status(200)
            .with_body(include_bytes!("./fixtures/sample_dependency.tar.gz"))
            .create();

        // NOTE(@ostera): this mock will be used to download the manifest
        let _package_manifest_mock = mockito::mock("GET", "/tricorder/beam/manifest.json")
            .with_status(200)
            .with_body(
                r#"
{
    "published_at": "2023-03-01T21:09:32+00:00",
    "keys": {
        "aarch64-apple-darwin": [ "a-hash", "b-hash" ],
        "x86_64-apple-darwin": [ "a-hash", "b-hash" ],
        "aarch64-unknown-linux-gnu": [ "a-hash", "b-hash" ],
        "x86_64-unknown-linux-gnu": [ "a-hash", "b-hash" ]
    }
}
                "#,
            )
            .create();

        let am = ArchiveManager::new(&config).into();
        let store = DefaultStore::new(config.clone(), am).into();

        #[derive(Debug)]
        pub struct NoopTricorder;
        #[async_trait]
        impl Tricorder for NoopTricorder {
            async fn connect(_conn: Connection) -> Result<Self, TricorderError> {
                Ok(Self)
            }

            async fn ensure_ready(&mut self) -> Result<(), TricorderError> {
                Ok(())
            }

            async fn generate_signature(
                &mut self,
                _: &ConcreteTarget,
            ) -> Result<SignatureGenerationFlow, TricorderError> {
                unreachable!();
            }
        }

        let mgr: TricorderManager<NoopTricorder, _> = TricorderManager::new(config, store);

        let path: PathBuf = "./sample/file.exs".into();
        let t: Target = path.as_path().into();
        let target_registry = TargetRegistry::new();
        let target_id = target_registry.register_target(&t);
        let ct = ConcreteTarget::new(Goal::Build, target_id, t.into(), path);
        mgr.find_and_ready(&ct).await.unwrap();

        assert!(warp_root.child("store/a-hash/tricorder.exe").exists());
    }

    #[tokio::test]
    async fn fails_if_tricorder_cannot_be_executed() {
        let warp_root = assert_fs::TempDir::new().unwrap();
        // NOTE(@ostera): this line is useful for debugging the output directory when something
        // goes wrong.
        let warp_root = warp_root.into_persistent();
        dbg!(&warp_root.path());

        let config = Config::builder()
            .warp_root(warp_root.path().to_path_buf())
            .public_store_cdn_url(mockito::server_url().parse().unwrap())
            .public_store_metadata_url(mockito::server_url().parse().unwrap())
            .build()
            .unwrap();

        // NOTE(@ostera): this mock will be used by the public store to fetch an actual artifact
        let _public_store_mock2 = mockito::mock("GET", "/b-hash.tar.gz")
            .with_status(200)
            .with_body(include_bytes!("./fixtures/sample_dependency.tar.gz"))
            .create();

        // NOTE(@ostera): this mock will be used to download the manifest
        let _package_manifest_mock = mockito::mock("GET", "/tricorder/beam/manifest.json")
            .with_status(200)
            .with_body(
                r#"
{
    "published_at": "2023-03-01T21:09:32+00:00",
    "keys": {
        "aarch64-apple-darwin": [  "b-hash" ],
        "x86_64-apple-darwin": [  "b-hash" ],
        "aarch64-unknown-linux-gnu": [  "b-hash" ],
        "x86_64-unknown-linux-gnu": [  "b-hash" ]
    }
}
                "#,
            )
            .create();

        let am = ArchiveManager::new(&config).into();
        let store = DefaultStore::new(config.clone(), am).into();

        #[derive(Debug)]
        pub struct UnreachableTricorder;
        #[async_trait]
        impl Tricorder for UnreachableTricorder {
            async fn connect(_conn: Connection) -> Result<Self, TricorderError> {
                unreachable!();
            }

            async fn ensure_ready(&mut self) -> Result<(), TricorderError> {
                unreachable!();
            }

            async fn generate_signature(
                &mut self,
                _: &ConcreteTarget,
            ) -> Result<SignatureGenerationFlow, TricorderError> {
                unreachable!();
            }
        }

        let mgr: TricorderManager<UnreachableTricorder, _> = TricorderManager::new(config, store);

        let path: PathBuf = "./sample/file.exs".into();
        let t: Target = path.as_path().into();
        let target_registry = TargetRegistry::new();
        let target_id = target_registry.register_target(&t);
        let ct = ConcreteTarget::new(Goal::Build, target_id, t.into(), path);
        let err = mgr.find_and_ready(&ct).await.unwrap_err();

        assert_matches!(
            err,
            TricorderManagerError::ProcessPoolError(ProcessPoolError::CommandSpawningError(_))
        );
    }

    #[tokio::test]
    async fn fails_if_tricorder_cannot_be_connected() {
        let warp_root = assert_fs::TempDir::new().unwrap();
        // NOTE(@ostera): this line is useful for debugging the output directory when something
        // goes wrong.
        let warp_root = warp_root.into_persistent();
        dbg!(&warp_root.path());

        let config = Config::builder()
            .warp_root(warp_root.path().to_path_buf())
            .public_store_cdn_url(mockito::server_url().parse().unwrap())
            .public_store_metadata_url(mockito::server_url().parse().unwrap())
            .build()
            .unwrap();

        // NOTE(@ostera): this mock will be used by the public store to fetch an actual artifact
        let _public_store_mock1 = mockito::mock("GET", "/a-hash.tar.gz")
            .with_status(200)
            .with_body(include_bytes!("./fixtures/sample_artifact.tar.gz"))
            .create();

        let _public_store_mock2 = mockito::mock("GET", "/b-hash.tar.gz")
            .with_status(200)
            .with_body(include_bytes!("./fixtures/sample_dependency.tar.gz"))
            .create();

        // NOTE(@ostera): this mock will be used to download the manifest
        let _package_manifest_mock = mockito::mock("GET", "/tricorder/beam/manifest.json")
            .with_status(200)
            .with_body(
                r#"
{
    "published_at": "2023-03-01T21:09:32+00:00",
    "keys": {
        "aarch64-apple-darwin": [ "a-hash", "b-hash" ],
        "x86_64-apple-darwin": [ "a-hash", "b-hash" ],
        "aarch64-unknown-linux-gnu": [ "a-hash", "b-hash" ],
        "x86_64-unknown-linux-gnu": [ "a-hash", "b-hash" ]
    }
}
                "#,
            )
            .create();

        let am = ArchiveManager::new(&config).into();
        let store = DefaultStore::new(config.clone(), am).into();

        #[derive(Debug)]
        pub struct UnconnectableTricorder;
        #[async_trait]
        impl Tricorder for UnconnectableTricorder {
            async fn connect(_conn: Connection) -> Result<Self, TricorderError> {
                Err(TricorderError::Unknown)
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

        let mgr: TricorderManager<UnconnectableTricorder, _> = TricorderManager::new(config, store);

        let path: PathBuf = "./sample/file.exs".into();
        let t: Target = path.as_path().into();
        let target_registry = TargetRegistry::new();
        let target_id = target_registry.register_target(&t);
        let ct = ConcreteTarget::new(Goal::Build, target_id, t.into(), path);
        let err = mgr.find_and_ready(&ct).await.unwrap_err();

        assert_matches!(
            err,
            TricorderManagerError::TricorderError(TricorderError::Unknown)
        );
    }

    #[tokio::test]
    async fn fails_if_tricorder_cannot_be_readied() {
        let warp_root = assert_fs::TempDir::new().unwrap();
        // NOTE(@ostera): this line is useful for debugging the output directory when something
        // goes wrong.
        let warp_root = warp_root.into_persistent();
        dbg!(&warp_root.path());

        let config = Config::builder()
            .warp_root(warp_root.path().to_path_buf())
            .public_store_cdn_url(mockito::server_url().parse().unwrap())
            .public_store_metadata_url(mockito::server_url().parse().unwrap())
            .build()
            .unwrap();

        // NOTE(@ostera): this mock will be used by the public store to fetch an actual artifact
        let _public_store_mock1 = mockito::mock("GET", "/a-hash.tar.gz")
            .with_status(200)
            .with_body(include_bytes!("./fixtures/sample_artifact.tar.gz"))
            .create();

        let _public_store_mock2 = mockito::mock("GET", "/b-hash.tar.gz")
            .with_status(200)
            .with_body(include_bytes!("./fixtures/sample_dependency.tar.gz"))
            .create();

        // NOTE(@ostera): this mock will be used to download the manifest
        let _package_manifest_mock = mockito::mock("GET", "/tricorder/beam/manifest.json")
            .with_status(200)
            .with_body(
                r#"
{
    "published_at": "2023-03-01T21:09:32+00:00",
    "keys": {
        "aarch64-apple-darwin": [ "a-hash", "b-hash" ],
        "x86_64-apple-darwin": [ "a-hash", "b-hash" ],
        "aarch64-unknown-linux-gnu": [ "a-hash", "b-hash" ],
        "x86_64-unknown-linux-gnu": [ "a-hash", "b-hash" ]
    }
}
                "#,
            )
            .create();

        let am = ArchiveManager::new(&config).into();
        let store = DefaultStore::new(config.clone(), am).into();

        #[derive(Debug)]
        pub struct UnreadiableTricorder;
        #[async_trait]
        impl Tricorder for UnreadiableTricorder {
            async fn connect(_conn: Connection) -> Result<Self, TricorderError> {
                Ok(Self)
            }

            async fn ensure_ready(&mut self) -> Result<(), TricorderError> {
                Err(TricorderError::Unknown)
            }

            async fn generate_signature(
                &mut self,
                _: &ConcreteTarget,
            ) -> Result<SignatureGenerationFlow, TricorderError> {
                unreachable!();
            }
        }

        let mgr: TricorderManager<UnreadiableTricorder, _> = TricorderManager::new(config, store);

        let path: PathBuf = "./sample/file.exs".into();
        let t: Target = path.as_path().into();
        let target_registry = TargetRegistry::new();
        let target_id = target_registry.register_target(&t);
        let ct = ConcreteTarget::new(Goal::Build, target_id, t.into(), path);
        let err = mgr.find_and_ready(&ct).await.unwrap_err();

        assert_matches!(
            err,
            TricorderManagerError::TricorderError(TricorderError::Unknown)
        );
    }
}
