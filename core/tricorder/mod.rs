mod grpc;
mod manager;
mod registry;

pub use grpc::*;
pub use manager::*;
pub use registry::*;

use crate::archive::Archive;
use crate::model::{
    ConcreteTarget, ExecutableSpec, Requirement, Signature, SignatureError, TargetId,
};
use crate::store::ArtifactManifest;
use crate::sync::*;
use async_trait::async_trait;
use std::fmt::Debug;
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
    async fn connect(connection: Connection) -> Result<Self, TricorderError>
    where
        Self: Sized;

    async fn ensure_ready(&mut self) -> Result<(), TricorderError>;

    async fn generate_signature(
        &mut self,
        concrete_target: &ConcreteTarget,
        dependencies: &[(Arc<ExecutableSpec>, Arc<ArtifactManifest>)],
    ) -> Result<SignatureGenerationFlow, TricorderError>;

    async fn ready_dependency(
        &mut self,
        concrete_target: &ConcreteTarget,
        archive: &Archive,
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
pub enum SignatureGenerationFlow {
    GeneratedSignatures { signatures: Vec<Signature> },
    MissingRequirements { requirements: Vec<Requirement> },
    IgnoredTarget(TargetId),
}
