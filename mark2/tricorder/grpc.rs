use super::{SignatureGenerationFlow, Tricorder, TricorderError};
use crate::{resolver::ConcreteTarget, util::process_pool::ProcessSpec};
use async_trait::async_trait;

/// Protobuf generated code.
mod proto {
    include!(concat!(env!("OUT_DIR"), "/_include.rs"));
}

pub struct GrpcTricorder {
    process_spec: ProcessSpec<Self>,
    client: proto::build::warp::tricorder::tricorder_service_client::TricorderServiceClient<
        tonic::transport::Channel,
    >,
}

#[async_trait]
impl Tricorder for GrpcTricorder {
    fn process_spec(&self) -> &ProcessSpec<Self> {
        &self.process_spec
    }

    async fn ensure_ready(&self) -> Result<(), TricorderError> {
        Err(TricorderError::Unknown)
    }

    async fn generate_signature(
        &mut self,
        concrete_target: &ConcreteTarget,
    ) -> Result<SignatureGenerationFlow, TricorderError> {
        let request = proto::build::warp::tricorder::GenerateSignatureRequest {
            workspace_root: Default::default(),
            file: concrete_target.path().to_string_lossy().to_string(),
            symbol: None,
            dependencies: vec![],
        };

        let response = self
            .client
            .generate_signature(request)
            .await?
            .into_inner()
            .response
            .unwrap();

        todo!();
    }
}

impl From<tonic::Status> for TricorderError {
    fn from(value: tonic::Status) -> Self {
        TricorderError::GrpcError(value)
    }
}
