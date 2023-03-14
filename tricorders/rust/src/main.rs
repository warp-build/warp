mod proto {
    include!("./gen/_include.rs");
}
mod tricorder_service;
mod generate_signature;
mod get_ast;
mod rs_generate_signature;

use crate::tricorder_service::TricorderServiceImpl;
use crate::proto::build::warp::tricorder::tricorder_service_server::TricorderServiceServer;
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
