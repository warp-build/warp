/// A unique identifier for a target. It can only be constructed via `TargetRegistry::register`.
///
#[derive(Copy, Default, Debug, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct TargetId(u128);

impl std::fmt::Display for TargetId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl TargetId {
    pub(crate) fn next() -> Self {
        Self(uuid::Uuid::new_v4().to_u128_le())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    impl quickcheck::Arbitrary for TargetId {
        fn arbitrary(_g: &mut quickcheck::Gen) -> Self {
            TargetId::next()
        }
    }
}
