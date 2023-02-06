//! # Warp Drive Mark II
//!
//! This crate implements the build system in [RFC001: A Build System from the Future](https://www.notion.so/warp-build/RFC001-A-Build-System-from-the-Future-6e1d61d44cc3435881887c7e7c899d07).
//!

/// Protobuf generated code.
mod proto {
    include!(concat!(env!("OUT_DIR"), "/_include.rs"));
}

mod events;
mod executor;
mod planner;
mod resolver;
mod rules;
mod worker;

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
