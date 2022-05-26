use super::*;

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Event {
    BuildCompleted,
    BuildingTarget(Label),
    TargetBuilt(Label),
    CacheHit(Label),
}
