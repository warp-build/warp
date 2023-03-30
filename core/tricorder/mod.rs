mod context;
mod grpc;
mod manager;
mod registry;

pub use context::*;
pub use grpc::*;
pub use manager::*;
pub use registry::*;

use crate::archive::Archive;
use crate::model::{
    ConcreteTarget, ExecutableSpec, Requirement, Signature, SignatureError, TargetId, Task,
    TestMatcher,
};
use crate::store::ArtifactManifest;
use crate::sync::*;
use async_trait::async_trait;
use std::fmt::Debug;
use std::path::{Path, PathBuf};
use thiserror::*;
use url::Url;

const DEFAULT_TRICODER_BINARY_NAME: &str = "tricorder";

#[derive(Debug, Clone)]
pub struct Connection {
    pub(crate) tricorder_url: Url,
    pub(crate) port: u16,
}

#[async_trait]
pub trait Tricorder: Send + Sync + Debug {
    async fn connect(connection: Connection, ctx: TricorderContext) -> Result<Self, TricorderError>
    where
        Self: Sized;

    async fn ensure_ready(&mut self) -> Result<(), TricorderError>;

    async fn generate_signature(
        &mut self,
        concrete_target: &ConcreteTarget,
        dependencies: &[(Task, Arc<ExecutableSpec>, Arc<ArtifactManifest>)],
        test_matcher: Option<Arc<TestMatcher>>,
    ) -> Result<SignatureGenerationFlow, TricorderError>;

    async fn ready_dependency(
        &mut self,
        concrete_target: &ConcreteTarget,
        archive: &Archive,
    ) -> Result<SignatureGenerationFlow, TricorderError>;

    async fn get_ast(
        &mut self,
        concrete_target: &ConcreteTarget,
        dependencies: &[(Task, Arc<ExecutableSpec>, Arc<ArtifactManifest>)],
        test_matcher: &TestMatcher,
    ) -> Result<SignatureGenerationFlow, TricorderError>;
}

#[derive(Error, Debug)]
pub enum TricorderError {
    #[error("Something went wrong with this tricorder")]
    Unknown,

    #[error("Something went wrong with a gRPC tricorder: {0:?}")]
    GrpcError(tonic::Status),

    #[error(transparent)]
    SignatureError(SignatureError),
}

#[derive(Debug)]
pub struct Subtree {
    file: PathBuf,
    source_chunk: String,
    ast_hash: String,
    signature_name: String,
}

impl Subtree {
    pub fn source_chunk(&self) -> &str {
        self.source_chunk.as_ref()
    }

    pub fn ast_hash(&self) -> &str {
        self.ast_hash.as_ref()
    }

    pub fn signature_name(&self) -> &str {
        self.signature_name.as_ref()
    }

    pub fn file(&self) -> &Path {
        &self.file
    }
}

#[derive(Debug)]
pub enum SignatureGenerationFlow {
    GeneratedSignatures { signatures: Vec<Signature> },
    ExtractedAst { subtrees: Vec<Subtree> },
    MissingRequirements { requirements: Vec<Requirement> },
    IgnoredTarget(TargetId),
}
