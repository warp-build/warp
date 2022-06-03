use super::*;
use std::path::PathBuf;

#[derive(Clone, Debug)]
pub enum Event {
    BuildCompleted,
    RequeueingTarget(Label, Vec<Label>),
    BuildingTarget { label: Label, rule_mnemonic: String },
    ArchiveDownloading { label: Label, url: String },
    ArchiveVerifying(Label),
    ArchiveUnpacking(Label),
    ActionRunning { label: Label, action: Action },
    TargetBuilt(Label),
    CacheHit(Label, PathBuf),
}
