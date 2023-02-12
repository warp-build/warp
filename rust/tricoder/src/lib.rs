mod proto {
    include!(concat!(env!("OUT_DIR"), "/_include.rs"));
}

mod analyzer_service;
mod get_ast;

pub use analyzer_service::*;
pub use proto::*;
pub use get_ast::*;
