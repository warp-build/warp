mod proto {
    include!(concat!(env!("OUT_DIR"), "/_include.rs"));
}
mod analyzer_service;
mod generate_signature;
mod get_ast;
mod rs_generate_signature;

use crate::analyzer_service::AnalyzerServiceImpl;
use crate::proto::build::warp::codedb::analyzer_service_server::AnalyzerServiceServer;
use tonic::transport::Server;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let port = 1024;
    let addr = format!("0.0.0.0:{}", port).parse().unwrap();
    let server = AnalyzerServiceImpl::default();

    println!("Started gRPC server on 0.0.0.0:{}", port);

    Server::builder()
        .add_service(AnalyzerServiceServer::new(server))
        .serve(addr)
        .await?;

    Ok(())
}
