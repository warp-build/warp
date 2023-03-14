include!(concat!(env!("OUT_DIR"), "/_include.rs"));

use std::collections::HashMap;

use build::warp::tricorder::generate_signature_response;
use build::warp::tricorder::get_ast_response;
use build::warp::tricorder::tricorder_service_server::{TricorderService, TricorderServiceServer};
use build::warp::tricorder::*;
use build::warp::Signature;
use tonic::{transport::Server, Request, Response, Status};

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
                        rule: "test_rule".to_string(),
                        name: req.file.clone(),
                        deps: vec![],
                        runtime_deps: vec![],
                        config: Some(google::protobuf::Struct {
                            fields: {
                                let mut config = HashMap::default();
                                config.insert(
                                    "srcs".to_string(),
                                    google::protobuf::Value {
                                        kind: Some(google::protobuf::value::Kind::ListValue(
                                            google::protobuf::ListValue {
                                                values: vec![google::protobuf::Value {
                                                    kind: Some(
                                                        google::protobuf::value::Kind::StringValue(
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
