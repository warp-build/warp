use self::proto::build::warp::tricorder::generate_signature_response::Response;
use self::proto::build::warp::tricorder::EnsureReadyRequest;
use super::{Connection, SignatureGenerationFlow, Tricorder, TricorderError};
use crate::model::{rule, ConcreteTarget, Signature, SignatureError};
use async_trait::async_trait;
use tracing::instrument;

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
    #[instrument(name = "GrpcTricorder::connect")]
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

    #[instrument(name = "GrpcTricorder::ensure_ready", skip(self))]
    async fn ensure_ready(&mut self) -> Result<(), TricorderError> {
        let req = EnsureReadyRequest::default();
        let _ = self.client.ensure_ready(req).await?;
        Ok(())
    }

    #[instrument(
        name = "GrpcTricorder::generate_signature",
        skip(self, concrete_target)
    )]
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
                for mut proto_sig in res.signatures.into_iter() {
                    let mut config: rule::Config = proto_sig.config.take().unwrap().into();
                    config.insert("name".to_string(), rule::Value::String(proto_sig.name));

                    let sig = Signature::builder()
                        .rule(proto_sig.rule)
                        .target(concrete_target.clone())
                        .config(config)
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

impl From<proto::google::protobuf::Struct> for rule::Config {
    fn from(values: proto::google::protobuf::Struct) -> Self {
        let mut cfg = Self::default();
        for (k, v) in values.fields {
            cfg.insert(k, v.into())
        }
        cfg
    }
}

impl From<proto::google::protobuf::Value> for rule::Value {
    fn from(val: proto::google::protobuf::Value) -> Self {
        match val.kind.unwrap() {
            proto::google::protobuf::value::Kind::StringValue(k) => Self::String(k),
            proto::google::protobuf::value::Kind::ListValue(l) => l.into(),
            proto::google::protobuf::value::Kind::NullValue(_) => unimplemented!(),
            proto::google::protobuf::value::Kind::NumberValue(_) => unimplemented!(),
            proto::google::protobuf::value::Kind::BoolValue(_) => unimplemented!(),
            proto::google::protobuf::value::Kind::StructValue(_) => unimplemented!(),
        }
    }
}

impl From<proto::google::protobuf::ListValue> for rule::Value {
    fn from(list: proto::google::protobuf::ListValue) -> Self {
        let mut items = vec![];
        for item in list.values {
            items.push(item.into());
        }
        Self::List(items)
    }
}
