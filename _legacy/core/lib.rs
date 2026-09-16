//! # Warp Drive Mark II
//!
//! The flow begins by creating a `Config` struct and using it to build a new `WarpDrive`
//! struct. From there, `WarpDrive` will orchestrate a `SharedContext` and a `WorkerPool` to
//! execute any number of `Target`s and return a reference to a collection of `TaskResults`.
//!
//! This keeps the internals of the Mark II private, while still making it possible to post-process
//! the results of the execution.
//!

pub(crate) mod archive;
pub mod code;
pub(crate) mod config;
pub(crate) mod drive;
pub mod events;
pub(crate) mod executor;
pub(crate) mod model;
pub(crate) mod os;
pub(crate) mod planner;
pub(crate) mod resolver;
pub(crate) mod rules;
pub(crate) mod store;
pub(crate) mod sync;
pub(crate) mod testing;
pub(crate) mod tricorder;
pub(crate) mod util;
pub(crate) mod worker;
pub(crate) mod workspace;

pub use config::*;
pub use drive::*;
pub use model::{CacheStatus, Goal, Target, TaskId};

#[macro_use]
extern crate derive_builder;

#[cfg(test)]
#[macro_use]
extern crate assert_matches;

#[cfg(test)]
extern crate quickcheck;
#[cfg(test)]
#[macro_use(quickcheck)]
extern crate quickcheck_macros;
