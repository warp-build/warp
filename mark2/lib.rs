//! # Warp Drive Mark II
//!
//! This crate implements the build system in [RFC001: A Build System from the Future](https://www.notion.so/warp-build/RFC001-A-Build-System-from-the-Future-6e1d61d44cc3435881887c7e7c899d07).
//!
//! The flow begins by creating a `Config` struct and using it to build a new `WarpDrive`
//! struct. From there, `WarpDrive` will orchestrate a `SharedContext` and a `WorkerPool` to
//! execute any number of `Target`s and return a reference to a collection of `TaskResults`.
//!
//! This keeps the internals of the Mark II private, while still making it possible to post-process
//! the results of the execution.
//!

mod archive;
mod config;
mod drive;
mod events;
mod executor;
mod planner;
mod resolver;
mod rules;
mod store;
mod sync;
mod tricorder;
mod util;
mod worker;
mod workspace;

pub use config::*;
pub use drive::*;

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
