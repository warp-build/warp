mod manager;

pub use manager::*;

use async_trait::async_trait;
use thiserror::*;

#[derive(Error, Debug)]
pub enum TricorderError {}

pub enum SignatureGenerationFlow {
    GeneratedSignatures { signatures: Vec<Signature> },
    MissingRequirements { requirements: Vec<Requirement> },
}

#[async_trait]
pub trait Tricorder {
    async fn ensure_ready(&self) -> Result<(), TricorderError>;

    async fn generate_signature() -> Result<SignatureGenerationFlow, TricorderError>;
}
