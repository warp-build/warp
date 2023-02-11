mod proto {
    include!(concat!(env!("OUT_DIR"), "/_include.rs"));
}

mod analyzer_service;

pub use analyzer_service::*;
pub use proto::*;
