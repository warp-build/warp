mod tricorder_service;
use tricorder::build::warp::tricorder::tricorder_service_server::TricorderServiceServer;
use crate::tricorder_service::TricorderServiceImpl;
use tonic::transport::Server;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let port: i32 = std::env::args().nth(2).unwrap().parse().unwrap();
    let addr = format!("0.0.0.0:{}", port).parse().unwrap();
    let server = TricorderServiceImpl::default();

    println!("Started gRPC server on 0.0.0.0:{}", port);

    Server::builder()
        .add_service(TricorderServiceServer::new(server))
        .serve(addr)
        .await?;

    Ok(())
}