use super::*;
use std::path::PathBuf;

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Event {
    BuildCompleted,
    RequeueingTarget(Label, Vec<Label>),
    BuildingTarget(Label),
    TargetBuilt(Label),
    CacheHit(Label, PathBuf),
}
