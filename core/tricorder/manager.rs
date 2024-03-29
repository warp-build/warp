use super::{
    Connection, Tricorder, TricorderContext, TricorderError, TricorderRegistry,
    TricorderRegistryError, DEFAULT_TRICODER_BINARY_NAME,
};
use crate::events::event::TricorderEvent;
use crate::store::{ArtifactManifest, Store, StoreError};
use crate::sync::*;
use crate::util::port_finder::PortFinder;
use crate::util::process_pool::{ProcessId, ProcessPool, ProcessPoolError, ProcessSpec};

use crate::Config;
use dashmap::DashMap;
use std::collections::HashMap;
use std::marker::PhantomData;
use std::path::Path;
use thiserror::*;
use tracing::instrument;
use url::Url;

/// A manager of Tricorder processes and creator of clients. This struct keeps a thread-safe pool
/// of processes that can be used to create new clients for existing tricorders whenever needed.
///
#[derive(Clone)]
pub struct TricorderManager<S: Store, T: Tricorder> {
    artifact_store: Arc<S>,
    config: Config,
    ctx: TricorderContext,
    process_pool: ProcessPool<T>,
    registry: TricorderRegistry,
    tricorders: DashMap<Url, (ProcessId<T>, Connection)>,

    // NOTE(@ostera): only used to serialize the calls to `next` and prevent fetching the same
    // target twice.
    _lock: Arc<tokio::sync::Mutex<()>>,
}

impl<S, T> TricorderManager<S, T>
where
    T: Tricorder + 'static,
    S: Store,
{
    pub fn new(config: Config, artifact_store: Arc<S>, ctx: TricorderContext) -> Self {
        Self {
            _lock: Arc::new(tokio::sync::Mutex::new(())),
            artifact_store,
            ctx,
            process_pool: ProcessPool::new(),
            registry: TricorderRegistry::new(config.clone()),
            config,
            tricorders: Default::default(),
        }
    }

    #[instrument(name = "TricorderManager::find_and_ready_by_path", skip(self))]
    pub async fn find_and_ready_by_path(
        &self,
        path: &Path,
    ) -> Result<Option<impl Tricorder>, TricorderManagerError> {
        if let Some(url) = self.registry.find_by_path(path)? {
            self.find_and_ready(&url).await
        } else {
            Ok(None)
        }
    }

    #[instrument(name = "TricorderManager::find_and_ready", skip(self))]
    pub async fn find_and_ready(
        &self,
        tricorder_url: &Url,
    ) -> Result<Option<impl Tricorder>, TricorderManagerError> {
        let _lock = self._lock.lock().await;

        let ec = self.config.event_channel();
        if let Some(entry) = self.tricorders.get(tricorder_url) {
            let (_pid, conn) = &*entry;
            let tricorder = T::connect(conn.clone(), self.ctx.clone()).await?;
            ec.send(TricorderEvent::TricorderConnectionEstablished {
                tricorder_url: tricorder_url.clone(),
            });
            return Ok(Some(tricorder));
        }

        // 2. install it
        let artifact_manifest = self
            .artifact_store
            .install_from_manifest_url(tricorder_url)
            .await?;

        // 3. start it
        let port = PortFinder::next().unwrap();
        let bin = self
            .artifact_store
            .canonicalize_provided_artifact(&artifact_manifest, DEFAULT_TRICODER_BINARY_NAME)
            .ok_or_else(|| TricorderManagerError::CouldNotFindTricoderBinary {
                artifact_manifest: artifact_manifest.clone(),
            })?;

        let env_locale = self
            .config
            .env()
            .get("LANG")
            .cloned()
            .unwrap_or("C.UTF-8".to_string());

        let mut shell_env: HashMap<String, String> = artifact_manifest
            .shell_env()
            .iter()
            .map(|(k, v)| (k.to_string(), v.to_string()))
            .collect();

        shell_env.insert("LANG".to_string(), env_locale);

        let workspace_root = self.config.workspace_root().to_string_lossy().to_string();

        let spec = ProcessSpec {
            bin: bin.to_path_buf(),
            args: vec!["start".to_string(), port.to_string(), workspace_root],
            env: shell_env,
            current_dir: Some(self.config.invocation_dir().to_path_buf()),
            _process_type: PhantomData,
        };
        let pid = self.process_pool.spawn(spec).await?;

        ec.send(TricorderEvent::TricorderServiceStarted {
            tricorder_url: tricorder_url.clone(),
        });

        // 4. connect to it
        let conn = Connection {
            tricorder_url: tricorder_url.clone(),
            port,
        };
        let mut tricorder = T::connect(conn.clone(), self.ctx.clone()).await?;

        ec.send(TricorderEvent::TricorderConnectionEstablished {
            tricorder_url: tricorder_url.clone(),
        });

        ec.send(TricorderEvent::TricorderReadyingStarted {
            tricorder_url: tricorder_url.clone(),
        });

        // 5. ready it
        tricorder.ensure_ready().await?;

        self.tricorders.insert(tricorder_url.clone(), (pid, conn));

        ec.send(TricorderEvent::TricorderReadyingCompleted {
            tricorder_url: tricorder_url.clone(),
        });

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
    use crate::archive::{Archive, ArchiveManager};
    use crate::model::{ConcreteTarget, ExecutableSpec, Goal, Target, Task, TestMatcher};
    use crate::resolver::TargetRegistry;
    use crate::store::DefaultStore;
    use crate::tricorder::{Connection, SignatureGenerationFlow};
    use crate::worker::TaskRegistry;
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
        dbg!(&warp_root.path());

        let mut server = mockito::Server::new_async().await;
        let mock_url = server.url().parse::<Url>().unwrap();
        let config = Config::builder()
            .warp_root(warp_root.path().to_path_buf())
            .public_store_cdn_url(mock_url.clone())
            .public_store_metadata_url(mock_url.clone())
            .build()
            .unwrap();

        // NOTE(@ostera): this mock will be used by the public store to fetch an actual artifact
        let _public_store_mock1 = server
            .mock("GET", "/a-hash.tar.gz")
            .with_status(200)
            .with_body(include_bytes!("./fixtures/sample_artifact.tar.gz"))
            .create_async()
            .await;

        let _public_store_mock2 = server
            .mock("GET", "/b-hash.tar.gz")
            .with_status(200)
            .with_body(include_bytes!("./fixtures/sample_dependency.tar.gz"))
            .create_async()
            .await;

        // NOTE(@ostera): this mock will be used to download the manifest
        let _package_manifest_mock = server
            .mock("GET", "/tricorders/beam/manifest.json")
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
            .create_async()
            .await;

        let am = ArchiveManager::new(&config).into();
        let store = DefaultStore::new(config.clone(), am).into();

        #[derive(Debug)]
        pub struct NoopTricorder;
        #[async_trait]
        impl Tricorder for NoopTricorder {
            async fn connect(
                _conn: Connection,
                _ctx: TricorderContext,
            ) -> Result<Self, TricorderError> {
                Ok(Self)
            }

            async fn ensure_ready(&mut self) -> Result<(), TricorderError> {
                Ok(())
            }

            async fn get_ast(
                &mut self,
                _: &ConcreteTarget,
                _: &[(Task, Arc<ExecutableSpec>, Arc<ArtifactManifest>)],
                _: &TestMatcher,
            ) -> Result<SignatureGenerationFlow, TricorderError> {
                unreachable!();
            }

            async fn generate_signature(
                &mut self,
                _: &ConcreteTarget,
                _: &[(Task, Arc<ExecutableSpec>, Arc<ArtifactManifest>)],
                _: Option<Arc<TestMatcher>>,
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

        let target_registry = Arc::new(TargetRegistry::new());
        let task_registry = Arc::new(TaskRegistry::new());
        let ctx = TricorderContext::new(target_registry.clone(), task_registry.clone());
        let mgr: TricorderManager<_, NoopTricorder> = TricorderManager::new(config, store, ctx);

        let path: PathBuf = "./sample/file.exs".into();
        let t: Target = path.as_path().into();
        let target_id = target_registry.register_target(&t);
        let ct = ConcreteTarget::new(Goal::Build, target_id, t.into(), path, ".".into());
        mgr.find_and_ready_by_path(ct.path()).await.unwrap();

        assert!(warp_root.child("store/a-hash/tricorder.exe").exists());
    }

    #[tokio::test]
    async fn fails_if_tricorder_cannot_be_executed() {
        let warp_root = assert_fs::TempDir::new().unwrap();
        // NOTE(@ostera): this line is useful for debugging the output directory when something
        // goes wrong.
        // let warp_root = warp_root.into_persistent();
        dbg!(&warp_root.path());

        let mut server = mockito::Server::new_async().await;
        let mock_url = server.url().parse::<Url>().unwrap();
        let config = Config::builder()
            .warp_root(warp_root.path().to_path_buf())
            .public_store_cdn_url(mock_url.clone())
            .public_store_metadata_url(mock_url.clone())
            .build()
            .unwrap();

        // NOTE(@ostera): this mock will be used by the public store to fetch an actual artifact
        let _public_store_mock2 = server
            .mock("GET", "/b-hash.tar.gz")
            .with_status(200)
            .with_body(include_bytes!("./fixtures/sample_dependency.tar.gz"))
            .create_async()
            .await;

        // NOTE(@ostera): this mock will be used to download the manifest
        let _package_manifest_mock = server
            .mock("GET", "/tricorders/beam/manifest.json")
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
            .create_async()
            .await;

        let am = ArchiveManager::new(&config).into();
        let store = DefaultStore::new(config.clone(), am).into();

        #[derive(Debug)]
        pub struct UnreachableTricorder;
        #[async_trait]
        impl Tricorder for UnreachableTricorder {
            async fn connect(
                _conn: Connection,
                _ctx: TricorderContext,
            ) -> Result<Self, TricorderError> {
                unreachable!();
            }

            async fn ensure_ready(&mut self) -> Result<(), TricorderError> {
                unreachable!();
            }

            async fn get_ast(
                &mut self,
                _: &ConcreteTarget,
                _: &[(Task, Arc<ExecutableSpec>, Arc<ArtifactManifest>)],
                _: &TestMatcher,
            ) -> Result<SignatureGenerationFlow, TricorderError> {
                unreachable!();
            }

            async fn generate_signature(
                &mut self,
                _: &ConcreteTarget,
                _: &[(Task, Arc<ExecutableSpec>, Arc<ArtifactManifest>)],
                _: Option<Arc<TestMatcher>>,
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
        let target_registry = Arc::new(TargetRegistry::new());
        let task_registry = Arc::new(TaskRegistry::new());
        let ctx = TricorderContext::new(target_registry.clone(), task_registry.clone());

        let mgr: TricorderManager<_, UnreachableTricorder> =
            TricorderManager::new(config, store, ctx);

        let path: PathBuf = "./sample/file.exs".into();
        let t: Target = path.as_path().into();
        let target_id = target_registry.register_target(&t);
        let ct = ConcreteTarget::new(Goal::Build, target_id, t.into(), path, ".".into());
        let err = mgr.find_and_ready_by_path(ct.path()).await.unwrap_err();

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

        let mut server = mockito::Server::new_async().await;
        let mock_url = server.url().parse::<Url>().unwrap();
        let config = Config::builder()
            .warp_root(warp_root.path().to_path_buf())
            .public_store_cdn_url(mock_url.clone())
            .public_store_metadata_url(mock_url.clone())
            .build()
            .unwrap();

        // NOTE(@ostera): this mock will be used by the public store to fetch an actual artifact
        let _public_store_mock1 = server
            .mock("GET", "/a-hash.tar.gz")
            .with_status(200)
            .with_body(include_bytes!("./fixtures/sample_artifact.tar.gz"))
            .create_async()
            .await;

        let _public_store_mock2 = server
            .mock("GET", "/b-hash.tar.gz")
            .with_status(200)
            .with_body(include_bytes!("./fixtures/sample_dependency.tar.gz"))
            .create_async()
            .await;

        // NOTE(@ostera): this mock will be used to download the manifest
        let _package_manifest_mock = server
            .mock("GET", "/tricorders/beam/manifest.json")
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
            .create_async()
            .await;

        let am = ArchiveManager::new(&config).into();
        let store = DefaultStore::new(config.clone(), am).into();

        #[derive(Debug)]
        pub struct UnconnectableTricorder;
        #[async_trait]
        impl Tricorder for UnconnectableTricorder {
            async fn connect(
                _conn: Connection,
                _ctx: TricorderContext,
            ) -> Result<Self, TricorderError> {
                Err(TricorderError::Unknown)
            }

            async fn ensure_ready(&mut self) -> Result<(), TricorderError> {
                unreachable!()
            }

            async fn get_ast(
                &mut self,
                _: &ConcreteTarget,
                _: &[(Task, Arc<ExecutableSpec>, Arc<ArtifactManifest>)],
                _: &TestMatcher,
            ) -> Result<SignatureGenerationFlow, TricorderError> {
                unreachable!();
            }

            async fn generate_signature(
                &mut self,
                _: &ConcreteTarget,
                _: &[(Task, Arc<ExecutableSpec>, Arc<ArtifactManifest>)],
                _: Option<Arc<TestMatcher>>,
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

        let target_registry = Arc::new(TargetRegistry::new());
        let task_registry = Arc::new(TaskRegistry::new());
        let ctx = TricorderContext::new(target_registry.clone(), task_registry.clone());

        let mgr: TricorderManager<_, UnconnectableTricorder> =
            TricorderManager::new(config, store, ctx);

        let path: PathBuf = "./sample/file.exs".into();
        let t: Target = path.as_path().into();
        let target_id = target_registry.register_target(&t);
        let ct = ConcreteTarget::new(Goal::Build, target_id, t.into(), path, ".".into());
        let err = mgr.find_and_ready_by_path(ct.path()).await.unwrap_err();

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

        let mut server = mockito::Server::new_async().await;
        let mock_url = server.url().parse::<Url>().unwrap();
        let config = Config::builder()
            .warp_root(warp_root.path().to_path_buf())
            .public_store_cdn_url(mock_url.clone())
            .public_store_metadata_url(mock_url.clone())
            .build()
            .unwrap();

        // NOTE(@ostera): this mock will be used by the public store to fetch an actual artifact
        let _public_store_mock1 = server
            .mock("GET", "/a-hash.tar.gz")
            .with_status(200)
            .with_body(include_bytes!("./fixtures/sample_artifact.tar.gz"))
            .create_async()
            .await;

        let _public_store_mock2 = server
            .mock("GET", "/b-hash.tar.gz")
            .with_status(200)
            .with_body(include_bytes!("./fixtures/sample_dependency.tar.gz"))
            .create_async()
            .await;

        // NOTE(@ostera): this mock will be used to download the manifest
        let _package_manifest_mock = server
            .mock("GET", "/tricorders/beam/manifest.json")
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
            .create_async()
            .await;

        let am = ArchiveManager::new(&config).into();
        let store = DefaultStore::new(config.clone(), am).into();

        #[derive(Debug)]
        pub struct UnreadiableTricorder;
        #[async_trait]
        impl Tricorder for UnreadiableTricorder {
            async fn connect(
                _conn: Connection,
                _ctx: TricorderContext,
            ) -> Result<Self, TricorderError> {
                Ok(Self)
            }

            async fn ensure_ready(&mut self) -> Result<(), TricorderError> {
                Err(TricorderError::Unknown)
            }

            async fn get_ast(
                &mut self,
                _: &ConcreteTarget,
                _: &[(Task, Arc<ExecutableSpec>, Arc<ArtifactManifest>)],
                _: &TestMatcher,
            ) -> Result<SignatureGenerationFlow, TricorderError> {
                unreachable!();
            }

            async fn generate_signature(
                &mut self,
                _: &ConcreteTarget,
                _: &[(Task, Arc<ExecutableSpec>, Arc<ArtifactManifest>)],
                _: Option<Arc<TestMatcher>>,
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

        let target_registry = Arc::new(TargetRegistry::new());
        let task_registry = Arc::new(TaskRegistry::new());
        let ctx = TricorderContext::new(target_registry.clone(), task_registry.clone());
        let mgr: TricorderManager<_, UnreadiableTricorder> =
            TricorderManager::new(config, store, ctx);

        let path: PathBuf = "./sample/file.exs".into();
        let t: Target = path.as_path().into();
        let target_id = target_registry.register_target(&t);
        let ct = ConcreteTarget::new(Goal::Build, target_id, t.into(), path, ".".into());
        let err = mgr.find_and_ready_by_path(ct.path()).await.unwrap_err();

        assert_matches!(
            err,
            TricorderManagerError::TricorderError(TricorderError::Unknown)
        );
    }
}
