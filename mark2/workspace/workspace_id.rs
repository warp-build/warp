#[derive(Copy, Default, Debug, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct WorkspaceId(u128);

impl WorkspaceId {
    pub(crate) fn next() -> Self {
        Self(uuid::Uuid::new_v4().to_u128_le())
    }
}
