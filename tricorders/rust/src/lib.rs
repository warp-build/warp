pub mod analysis;
mod dependencies;
mod error;
mod grpc;

pub use error::*;

#[macro_use]
extern crate derive_builder;

#[cfg(test)]
#[macro_use]
extern crate assert_matches;

use analysis::Analyzer;
use dependencies::DependencyManager;
use grpc::GrpcTricorder;
use std::net::SocketAddr;
use std::path::Path;
use std::sync::Arc;

static RUST_TRICORDER_URL_STR: &str = "https://store.warp.build/tricorders/rust/manifest.json";
static RUST_TRICORDER_URL: once_cell::sync::Lazy<url::Url> =
    once_cell::sync::Lazy::new(|| url::Url::parse(RUST_TRICORDER_URL_STR).unwrap());

/// The entrypoint to the Rust tricorder. This struct orchestrates the subcomponents and starts the
/// gRPC service that implements the Tricorder protocol.
///
pub struct Tricorder {
    dep_manager: Arc<DependencyManager>,
    service: GrpcTricorder,
}

impl Tricorder {
    pub fn new<P>(address: SocketAddr, root: P) -> Result<Self, TricorderError>
    where
        P: AsRef<Path>,
    {
        let root = root.as_ref().canonicalize()?;

        let dep_manager = Arc::new(DependencyManager::new(&root));
        let analyzer = Arc::new(Analyzer::new(dep_manager.clone()));

        let service = GrpcTricorder::builder()
            .dep_manager(dep_manager.clone())
            .analyzer(analyzer)
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
