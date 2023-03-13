mod proto {
    include!("./gen/_include.rs");
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
    let port: i32 = std::env::args().nth(2).unwrap().parse().unwrap();
    let addr = format!("0.0.0.0:{}", port).parse().unwrap();
    let server = AnalyzerServiceImpl::default();

    println!("Started gRPC server on 0.0.0.0:{}", port);

    Server::builder()
        .add_service(AnalyzerServiceServer::new(server))
        .serve(addr)
        .await?;

    Ok(())
}
