use super::*;
use dashmap::DashMap;
use std::sync::{Arc, Mutex};
use thiserror::*;
use tracing::*;

/// The Target Registry keeps track of all the targets being used in the system, all the targets that
/// have been aliased (due to reparenting, workspace changing, etc).
///
#[derive(Default, Debug, Clone)]
pub struct TargetRegistry {
    ids: DashMap<Arc<Target>, TargetId>,
    targets: DashMap<TargetId, Arc<Target>>,

    // NOTE(@ostera): only used to serialize the calls to `register_target` and prevent registering
    // the same target under two different ids.
    _register_lock: Arc<Mutex<()>>,
}

#[derive(Error, Debug)]
pub enum TargetRegistryError {}

impl TargetRegistry {
    pub fn new() -> Self {
        Self::default()
    }

    /// Get a handle for a Target that can be used to alias and unalias it later.
    ///
    /// If the target has already been registered, the same identifier is returned.
    ///
    #[tracing::instrument(name = "TargetRegistry::register", skip(self))]
    pub fn register_target<L>(&self, target: L) -> TargetId
    where
        L: AsRef<Target> + std::fmt::Debug,
    {
        let target = target.as_ref();
        let _lock = self._register_lock.lock().unwrap();
        if let Some(id) = self.find_target(target) {
            id
        } else {
            let target = Arc::new(target.to_owned());
            let id = TargetId::next();
            self.ids.insert(target.clone(), id);
            self.targets.insert(id, target);

            id
        }
    }

    /// Get a handle for a collection of targets. Behaves like `TargetRegistry::register`.
    ///
    #[tracing::instrument(name = "TargetRegistry::register", skip(self))]
    pub fn register_many_targets(&self, targets: &[Target]) -> Vec<TargetId> {
        let mut ids = vec![];
        for target in targets {
            ids.push(self.register_target(target));
        }
        ids
    }

    pub fn update_target<L>(&self, id: TargetId, target: L)
    where
        L: Into<Target>,
    {
        let _lock = self._register_lock.lock().unwrap();
        let target = target.into();
        if let Some(found_id) = self.find_target(&target) {
            if id == found_id {
                return;
            }
        }
        let target = Arc::new(target);

        self.ids.insert(target.clone(), id);
        self.targets.insert(id, target);
    }

    /// Find the id of a target that has already been registered.
    ///
    /// If the Target's corresponding TargetId has been aliased towards some other TargetId, the new
    /// TargetId will be returned.
    ///
    #[tracing::instrument(name = "TargetRegistry::find", skip(self))]
    pub fn find_target(&self, target: &Target) -> Option<TargetId> {
        self.ids.get(target).map(|r| *r.value())
    }

    #[tracing::instrument(name = "TargetRegistry::get", skip(self))]
    pub fn get_target(&self, id: TargetId) -> Arc<Target> {
        (*self.targets.get(&id).unwrap()).clone()
    }
}
