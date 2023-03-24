mod concrete_target;
mod dependencies;
mod env;
mod executable_spec;
mod goal;
mod provided_files;
mod requirement;
pub mod rule;
mod run_script;
mod signature;
mod source_set;
mod target;
mod target_id;
mod test_matcher;
mod test_matcher_id;

pub use concrete_target::*;
pub use dependencies::*;
pub use env::*;
pub use executable_spec::*;
pub use goal::*;
pub use provided_files::*;
pub use requirement::*;
pub use rule::{Pinned, Portability, Rule, RuleKind, RuleName};
pub use run_script::*;
pub use signature::*;
pub use source_set::*;
pub use target::*;
pub use target_id::*;
pub use test_matcher::*;
pub use test_matcher_id::*;

#[derive(Default, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum CacheStatus {
    #[default]
    Fresh,
    Cached,
}
