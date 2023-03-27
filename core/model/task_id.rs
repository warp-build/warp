use serde::de::Visitor;
use serde::{Deserialize, Serialize};

/// A unique identifier for a target. It should only be constructed via `TaskRegistry::register`.
///
#[derive(Copy, Default, Debug, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct TaskId(u128);

impl std::fmt::Display for TaskId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        "Task(".fmt(f)?;
        self.0.fmt(f)?;
        ")".fmt(f)?;
        Ok(())
    }
}

impl TaskId {
    pub(crate) fn next() -> Self {
        Self(uuid::Uuid::new_v4().to_u128_le())
    }
}

impl Serialize for TaskId {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(&uuid::Uuid::from_u128_le(self.0).to_string())
    }
}

struct TaskVisitor;

impl Visitor<'_> for TaskVisitor {
    type Value = TaskId;

    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Ok(TaskId(uuid::Uuid::parse_str(v).unwrap().to_u128_le()))
    }

    fn expecting(&self, _fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        todo!()
    }
}

impl<'de> Deserialize<'de> for TaskId {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_str(TaskVisitor)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    impl quickcheck::Arbitrary for TaskId {
        fn arbitrary(_g: &mut quickcheck::Gen) -> Self {
            TaskId::next()
        }
    }
}
