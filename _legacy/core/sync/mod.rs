//! Wrapper over standard sync primitives to allow for concurrent testing.
//!

#[cfg(all(shuttle, test))]
pub(crate) use shuttle::{sync::*, thread};

#[cfg(not(all(shuttle, test)))]
pub(crate) use std::sync::*;
