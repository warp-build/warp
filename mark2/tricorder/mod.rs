mod grpc;
mod manager;
mod registry;

pub use grpc::*;
pub use manager::*;
pub use registry::*;

use crate::model::ConcreteTarget;
use async_trait::async_trait;
use std::fmt::Debug;
use thiserror::*;

#[derive(Error, Debug)]
pub enum TricorderError {
    #[error("Something went wrong with this tricorder")]
    Unknown,

    #[error("Something went wrong with a gRPC tricorder: {0:?}")]
    GrpcError(tonic::Status),
}

pub enum SignatureGenerationFlow {
    //GeneratedSignatures { signatures: Vec<Signature> },
    //MissingRequirements { requirements: Vec<Requirement> },
}

const DEFAULT_TRICODER_BINARY_NAME: &str = "tricorder";

#[derive(Debug, Clone, Copy)]
pub struct Connection {
    port: u16,
}

#[async_trait]
pub trait Tricorder: Send + Sync + Debug {
    async fn connect(connection: Connection) -> Result<Self, TricorderError>
    where
        Self: Sized;

    async fn ensure_ready(&self) -> Result<(), TricorderError>;

    async fn generate_signature(
        &self,
        concrete_target: &ConcreteTarget,
    ) -> Result<SignatureGenerationFlow, TricorderError>;
}
