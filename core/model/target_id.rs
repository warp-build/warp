use serde::de::Visitor;
use serde::{Deserialize, Serialize};

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

impl Serialize for TargetId {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(&uuid::Uuid::from_u128_le(self.0).to_string())
    }
}

struct TargetVisitor;

impl Visitor<'_> for TargetVisitor {
    type Value = TargetId;

    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Ok(TargetId(uuid::Uuid::parse_str(v).unwrap().to_u128_le()))
    }

    fn expecting(&self, _fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        todo!()
    }
}

impl<'de> Deserialize<'de> for TargetId {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_str(TargetVisitor)
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
