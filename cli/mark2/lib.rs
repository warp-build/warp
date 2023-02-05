mod proto {
    include!(concat!(env!("OUT_DIR"), "/_include.rs"));
}

mod events;
mod executor;
mod model;
mod planner;
mod resolver;
mod rules;
mod worker;

#[cfg(test)]
#[macro_use]
extern crate assert_matches;

#[macro_use]
extern crate derive_builder;
