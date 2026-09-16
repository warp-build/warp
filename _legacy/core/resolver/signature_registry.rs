use crate::model::{Signature, SignatureId};
use crate::sync::Arc;
use dashmap::DashMap;
use tracing::{instrument, *};

/// A small registry to trade test matcher (which are non-Copyable objects) for Copyable
/// Ids.
///
#[derive(Default, Debug, Clone)]
pub struct SignatureRegistry {
    signatures: DashMap<SignatureId, Arc<Signature>>,
}

impl SignatureRegistry {
    pub fn new() -> Self {
        Self::default()
    }

    #[instrument(name = "SignatureRegistry::register", skip(self))]
    pub fn register(&self, signature: Signature) -> SignatureId {
        let id = SignatureId::next();
        self.signatures.insert(id, signature.into());
        id
    }

    #[instrument(name = "SignatureRegistry::register_many", skip(self))]
    pub fn register_many(&self, sigs: Vec<Signature>) -> Vec<SignatureId> {
        let mut ids = vec![];
        for sig in sigs {
            ids.push(self.register(sig));
        }
        ids
    }

    #[instrument(name = "TargetRegistry::get", skip(self))]
    pub fn get(&self, id: SignatureId) -> Arc<Signature> {
        (*self.signatures.get(&id).unwrap()).clone()
    }

    pub fn len(&self) -> usize {
        self.signatures.len()
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}
