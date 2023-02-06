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
