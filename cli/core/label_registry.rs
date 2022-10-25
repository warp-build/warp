use std::{
    fmt::Display,
    sync::{Arc, Mutex},
};

use super::*;
use dashmap::DashMap;
use thiserror::*;
use tracing::*;

/// A unique identifier for a label. It can only be constructed via `LabelRegistry::register`.
///
#[derive(Copy, Default, Debug, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct LabelId(u128);

impl Display for LabelId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl LabelId {
    fn next() -> Self {
        Self(uuid::Uuid::new_v4().to_u128_le())
    }
}

/// The Label Registry keeps track of all the labels being used in the system, all the labels that
/// have been aliased (due to reparenting, workspace changing, etc).
///
#[derive(Default, Debug, Clone)]
pub struct LabelRegistry {
    ids: DashMap<Label, LabelId>,
    labels: DashMap<LabelId, Label>,
    register_lock: Arc<Mutex<()>>,
}

#[derive(Error, Debug)]
pub enum LabelRegistryError {}

impl LabelRegistry {
    pub fn new() -> Self {
        Self::default()
    }

    /// Get a handle for a Label that can be used to alias and unalias it later.
    ///
    /// If the label has already been registered, the same identifier is returned.
    ///
    #[tracing::instrument(name = "LabelRegistry::register", skip(self))]
    pub fn register(&self, label: Label) -> LabelId {
        let _lock = self.register_lock.lock().unwrap();
        if let Some(id) = self.find(&label) {
            id
        } else {
            let id = LabelId::next();
            self.ids.insert(label.clone(), id);
            self.labels.insert(id, label);
            id
        }
    }

    /// Get a handle for a collection of labels. Behaves like `LabelRegistry::register`.
    ///
    #[tracing::instrument(name = "LabelRegistry::register", skip(self))]
    pub fn register_many(&self, labels: &[Label]) -> Vec<LabelId> {
        let mut ids = vec![];
        for label in labels {
            ids.push(self.register(label.clone()));
        }
        ids
    }

    pub fn update(&self, id: LabelId, label: Label) {
        let last_label = self.get(id);

        let label = last_label.promote(label);

        self.ids.insert(label.clone(), id);
        self.labels.insert(id, label);
    }

    /// Find the id of a label that has already been registered.
    ///
    /// If the Label's corresponding LabelId has been aliased towards some other LabelId, the new
    /// LabelId will be returned.
    ///
    #[tracing::instrument(name = "LabelRegistry::find", skip(self))]
    pub fn find(&self, label: &Label) -> Option<LabelId> {
        self.ids.get(label).map(|r| *r.value())
    }

    #[tracing::instrument(name = "LabelRegistry::get", skip(self))]
    pub fn get(&self, id: LabelId) -> Label {
        self.labels.get(&id).unwrap().clone()
    }
}
