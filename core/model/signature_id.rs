use serde::de::Visitor;
use serde::{Deserialize, Serialize};

/// A unique identifier for a signature. It should only be constructed via `SignatureRegistry::register`.
///
#[derive(Copy, Default, Debug, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct SignatureId(u128);

impl std::fmt::Display for SignatureId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl SignatureId {
    pub(crate) fn next() -> Self {
        Self(uuid::Uuid::new_v4().to_u128_le())
    }
}

impl Serialize for SignatureId {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(&uuid::Uuid::from_u128_le(self.0).to_string())
    }
}

struct SignatureVisitor;

impl Visitor<'_> for SignatureVisitor {
    type Value = SignatureId;

    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Ok(SignatureId(uuid::Uuid::parse_str(v).unwrap().to_u128_le()))
    }

    fn expecting(&self, _fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        todo!()
    }
}

impl<'de> Deserialize<'de> for SignatureId {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_str(SignatureVisitor)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    impl quickcheck::Arbitrary for SignatureId {
        fn arbitrary(_g: &mut quickcheck::Gen) -> Self {
            SignatureId::next()
        }
    }
}
