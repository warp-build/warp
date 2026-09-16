use super::*;
use crate::model::{ConcreteTarget, Target, TargetId};
use crate::sync::{Arc, Mutex};
use dashmap::DashMap;
use thiserror::*;
use tracing::*;

/// The Target Registry keeps track of all the targets being used in the system, all the targets that
/// have been aliased (due to reparenting, workspace changing, etc).
///
#[derive(Default, Debug, Clone)]
pub struct TargetRegistry {
    ids: DashMap<Arc<Target>, TargetId>,
    targets: DashMap<TargetId, Arc<Target>>,
    concrete_targets: DashMap<TargetId, Arc<ConcreteTarget>>,

    // NOTE(@ostera): only used to serialize the calls to `register_target` and prevent registering
    // the same target under two different ids.
    _register_lock: Arc<Mutex<()>>,
}

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
    pub fn register_many_targets<T>(&self, targets: &[T]) -> Vec<TargetId>
    where
        T: Into<Target> + Clone + std::fmt::Debug,
    {
        let mut ids = vec![];
        for target in targets {
            let target: Target = target.clone().into();
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

    #[tracing::instrument(name = "TargetRegistry::get", skip(self))]
    pub fn get_concrete_target(&self, id: TargetId) -> Arc<ConcreteTarget> {
        (*self.concrete_targets.get(&id).unwrap()).clone()
    }

    pub fn associate_concrete_target(
        &self,
        id: TargetId,
        ct: ConcreteTarget,
    ) -> Arc<ConcreteTarget> {
        let ct: Arc<ConcreteTarget> = ct.into();
        self.concrete_targets.insert(id, ct.clone());
        ct
    }

    pub fn len(&self) -> usize {
        self.ids.len()
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

#[derive(Error, Debug)]
pub enum TargetRegistryError {}

#[cfg(test)]
mod tests {
    use super::*;
    use quickcheck::*;

    #[cfg(shuttle)]
    #[test]
    fn conc_registering_the_same_target_returns_the_same_id() {
        use crate::sync::*;

        shuttle::check_dfs(
            move || {
                let mut gen = quickcheck::Gen::new(10);
                let target = Target::arbitrary(&mut gen);
                let reg = Arc::new(TargetRegistry::new());
                let id = reg.register_target(target.clone());

                let mut handles = vec![];
                for _ in 0..3 {
                    let reg = reg.clone();
                    let target = target.clone();
                    let handle = thread::spawn(move || {
                        reg.register_target(target.clone());
                    });
                    handles.push(handle);
                }

                for handle in handles {
                    handle.join().unwrap()
                }

                assert_eq!(id, reg.find_target(&target).unwrap());
                assert_eq!(reg.len(), 1);
            },
            None,
        );
    }

    #[quickcheck]
    fn searching_for_a_target_returns_a_valid_handle_if_the_target_is_registered(target: Target) {
        let reg = TargetRegistry::new();
        let handle = reg.register_target(target.clone());
        assert_eq!(reg.find_target(&target).unwrap(), handle);
    }

    #[quickcheck]
    fn searching_for_a_target_returns_nothing_if_the_target_is_not_registered(target: Target) {
        let reg = TargetRegistry::new();
        assert!(reg.find_target(&target).is_none());
    }

    #[quickcheck]
    #[should_panic]
    fn getting_a_target_with_an_unregistered_handle_is_a_panic(target_id: TargetId) {
        let reg = TargetRegistry::new();
        reg.get_target(target_id);
    }

    #[quickcheck]
    fn updating_a_handle_to_point_to_a_new_target(target_a: Target, target_b: Target) {
        let reg = TargetRegistry::new();
        let handle = reg.register_target(target_a.clone());
        assert_eq!(*reg.get_target(handle), target_a);
        reg.update_target(handle, target_b.clone());
        assert_eq!(*reg.get_target(handle), target_b);
    }

    #[quickcheck]
    fn registering_many_targets_skips_duplicates(target: Target) {
        let reg = TargetRegistry::new();
        // NOTE(@ostera): we are registering the same target twice
        let targets = &[target.clone(), target];
        let handles = reg.register_many_targets(targets);
        assert!(handles.len() <= targets.len());
        assert_eq!(handles[0], handles[1]);
    }
}
