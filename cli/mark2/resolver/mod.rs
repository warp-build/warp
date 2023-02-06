//! # Resolves what a given target points to in the project graph
//!
mod goal;
mod target;
mod target_id;
mod target_registry;

pub use goal::*;
pub use target::*;
pub use target_id::*;
pub use target_registry::*;
