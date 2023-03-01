mod grpc;
mod manager;
mod registry;

pub use grpc::*;
pub use manager::*;
pub use registry::*;

use crate::{resolver::ConcreteTarget, util::process_pool::ProcessSpec};
use async_trait::async_trait;
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

#[async_trait]
pub trait Tricorder {
    fn process_spec(&self) -> &ProcessSpec<Self>
    where
        Self: Sized;

    async fn ensure_ready(&self) -> Result<(), TricorderError>;

    async fn generate_signature(
        &mut self,
        concrete_target: &ConcreteTarget,
    ) -> Result<SignatureGenerationFlow, TricorderError>;
}
