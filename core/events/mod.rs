//! # Wire-friendly Build Execution Events
//!
//! None of these events include references to existing data, and they are all self-contained with
//! primitives that make them easy to encode in a wire-friendly format like Protobuf.
//!
//! NOTE(@ostera): This module is likely going to become the interface of a form of log-streamer
//! protocol that we can use to receive and print feedback from local and remote builds.
//!
mod channel;
mod consumer;
pub mod event;

pub use channel::*;
pub use consumer::*;
