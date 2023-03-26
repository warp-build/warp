mod proto {
    include!("./protos/_include.rs");
}

use proto::build::warp::tricorder::tricorder_service_server::{
    TricorderService, TricorderServiceServer,
};
use proto::build::warp::tricorder::{generate_signature_response, *};
use proto::build::warp::Signature;
use std::collections::HashMap;
use tonic::transport::Server;
use tonic::{Request, Response, Status};

#[derive(Default)]
struct TestTricorder;

#[tonic::async_trait]
impl TricorderService for TestTricorder {
    async fn ensure_ready(
        &self,
        _: Request<EnsureReadyRequest>,
    ) -> Result<Response<EnsureReadyResponse>, Status> {
        println!("Tricorder ready!");
        Ok(Response::new(EnsureReadyResponse::default()))
    }

    async fn prepare_dependency(
        &self,
        _req: Request<PrepareDependencyRequest>,
    ) -> Result<Response<PrepareDependencyResponse>, Status> {
        todo!()
    }

    async fn generate_signature(
        &self,
        req: Request<GenerateSignatureRequest>,
    ) -> Result<Response<GenerateSignatureResponse>, Status> {
        let req = req.into_inner();
        println!("Generating signature!");
        let res = GenerateSignatureResponse {
            response: Some(generate_signature_response::Response::Ok(
                GenerateSignatureSuccessResponse {
                    workspace_root: req.workspace_root,
                    file: req.file.clone(),
                    symbol: req.symbol,
                    signatures: vec![Signature {
                        rule: "rescript_library".to_string(),
                        name: req.file.clone(),
                        deps: vec![],
                        runtime_deps: vec![],
                        config: Some(proto::google::protobuf::Struct {
                            fields: {
                                let mut config = HashMap::default();
                                config.insert(
                                    "srcs".to_string(),
                                    proto::google::protobuf::Value {
                                        kind: Some(proto::google::protobuf::value::Kind::ListValue(
                                            proto::google::protobuf::ListValue {
                                                values: vec![proto::google::protobuf::Value {
                                                    kind: Some(
                                                        proto::google::protobuf::value::Kind::StringValue(
                                                            req.file,
                                                        ),
                                                    ),
                                                }],
                                            },
                                        )),
                                    },
                                );
                                config
                            },
                        }),
                    }],
                },
            )),
        };
        Ok(Response::new(res))
    }

    async fn get_ast(
        &self,
        _request: Request<GetAstRequest>,
    ) -> Result<Response<GetAstResponse>, Status> {
        Ok(Response::new(GetAstResponse::default()))
    }
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    dbg!(&std::env::args());
    let port: i32 = std::env::args()
        .into_iter()
        .nth(2)
        .unwrap()
        .parse()
        .unwrap();

    let addr = format!("0.0.0.0:{}", port).parse().unwrap();
    println!("Started gRPC server on 0.0.0.0:{}", port);

    Server::builder()
        .add_service(TricorderServiceServer::new(TestTricorder))
        .serve(addr)
        .await?;

    Ok(())
}
