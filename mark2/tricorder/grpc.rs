use super::{Connection, SignatureGenerationFlow, Tricorder, TricorderError};
use crate::model::ConcreteTarget;
use crate::sync::*;
use async_trait::async_trait;

/// Protobuf generated code.
mod proto {
    include!(concat!(env!("OUT_DIR"), "/_include.rs"));
}

/// An implementation of the Tricorder framework that works over gRPC.
///
#[derive(Debug)]
pub struct GrpcTricorder {
    conn: Connection,
    client: Arc<
        RwLock<
            proto::build::warp::tricorder::tricorder_service_client::TricorderServiceClient<
                tonic::transport::Channel,
            >,
        >,
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
        Ok(Self {
            conn,
            client: Arc::new(RwLock::new(client)),
        })
    }

    async fn ensure_ready(&self) -> Result<(), TricorderError> {
        Err(TricorderError::Unknown)
    }

    async fn generate_signature(
        &self,
        concrete_target: &ConcreteTarget,
    ) -> Result<SignatureGenerationFlow, TricorderError> {
        /*
        let request = proto::build::warp::tricorder::GenerateSignatureRequest {
            workspace_root: Default::default(),
            file: concrete_target.path().to_string_lossy().to_string(),
            symbol: None,
            dependencies: vec![],
        };

        let response = {
            let client = self.client.try_write().unwrap();
            client
                .generate_signature(request)
                .await?
                .into_inner()
                .response
                .unwrap();
        };
        */

        todo!();
    }
}

impl From<tonic::Status> for TricorderError {
    fn from(value: tonic::Status) -> Self {
        TricorderError::GrpcError(value)
    }
}
