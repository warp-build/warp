pub mod analysis;
mod dependencies;
mod error;
mod grpc;

pub use error::*;

#[macro_use]
extern crate derive_builder;
use dependencies::DependencyManager;
use grpc::GrpcTricorder;
use std::net::SocketAddr;
use std::sync::Arc;

/// The entrypoint to the Rust tricorder. This struct orchestrates the subcomponents and starts the
/// gRPC service that implements the Tricorder protocol.
///
pub struct Tricorder {
    dep_manager: Arc<DependencyManager>,
    service: GrpcTricorder,
}

impl Tricorder {
    pub fn new(address: SocketAddr) -> Result<Self, TricorderError> {
        let dep_manager = Arc::new(DependencyManager::default());

        let service = GrpcTricorder::builder()
            .dep_manager(dep_manager.clone())
            .address(address)
            .build()?;

        Ok(Self {
            dep_manager,
            service,
        })
    }

    pub async fn run(self) -> Result<(), TricorderError> {
        let (server_load, server) =
            futures::future::join(self.dep_manager.prepare(), self.service.serve()).await;

        server_load?;
        server?;

        Ok(())
    }
}
