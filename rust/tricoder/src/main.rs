use tonic::{transport::Server};
use tricoder::*;
use tricoder::build::warp::codedb::analyzer_service_server::AnalyzerServiceServer;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let port = 50051;
    let addr = format!("0.0.0.0:{}", port).parse().unwrap();
    let server = AnalyzerServiceImpl::default();

    println!("Started gRPC server on 0.0.0.0:{}", port);

    Server::builder()
        .add_service(AnalyzerServiceServer::new(server))
        .serve(addr)
        .await?;

    Ok(())
}
