//! # Implementation of a Rule Engine backed by an interpreted language.

mod executor;
mod store;

pub use executor::*;
use store::*;
