use self::proto::build::warp::tricorder::generate_signature_response::Response;
use self::proto::build::warp::tricorder::EnsureReadyRequest;
use super::{Connection, SignatureGenerationFlow, Tricorder, TricorderError};
use crate::model::{ConcreteTarget, Signature, SignatureError};
use crate::sync::*;
use async_trait::async_trait;

/// Protobuf generated code.
mod proto {
    include!(concat!(env!("OUT_DIR"), "/_include.rs"));
}

/// An implementation of the Tricorder framework that works over gRPC.
///
#[derive(Debug, Clone)]
pub struct GrpcTricorder {
    conn: Connection,
    client: proto::build::warp::tricorder::tricorder_service_client::TricorderServiceClient<
        tonic::transport::Channel,
    >,
}

#[async_trait]
impl Tricorder for GrpcTricorder {
    async fn connect(conn: Connection) -> Result<Self, TricorderError> {
        let conn_str = format!("http://0.0.0.0:{}", conn.port);
        let client = loop {
            let conn =
                proto::build::warp::tricorder::tricorder_service_client::TricorderServiceClient::connect(
                    conn_str.clone(),
                )
                .await;
            if let Ok(conn) = conn {
                break conn;
            }
            tokio::time::sleep(std::time::Duration::from_millis(1)).await;
        };
        Ok(Self { conn, client })
    }

    async fn ensure_ready(&mut self) -> Result<(), TricorderError> {
        let req = EnsureReadyRequest::default();
        let _ = self.client.ensure_ready(req).await?;
        Ok(())
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

        let res = self
            .client
            .generate_signature(request)
            .await?
            .into_inner()
            .response
            .unwrap();

        match res {
            Response::Ok(res) => {
                let mut signatures = vec![];
                for proto_sig in res.signatures.into_iter() {
                    let sig = Signature::builder()
                        .rule(proto_sig.rule)
                        .target(concrete_target.clone())
                        .build()?;
                    signatures.push(sig);
                }
                Ok(SignatureGenerationFlow::GeneratedSignatures { signatures })
            }
            Response::MissingDeps(_) => Ok(SignatureGenerationFlow::MissingRequirements {
                requirements: vec![],
            }),
        }
    }
}

impl From<tonic::Status> for TricorderError {
    fn from(value: tonic::Status) -> Self {
        TricorderError::GrpcError(value)
    }
}

impl From<SignatureError> for TricorderError {
    fn from(value: SignatureError) -> Self {
        TricorderError::SignatureError(value)
    }
}
