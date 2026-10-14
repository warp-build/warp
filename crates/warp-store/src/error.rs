use std::{error, fmt, io};

#[derive(Debug)]
pub struct StoreError(String);

impl StoreError {
    pub(crate) fn new(message: impl Into<String>) -> Self {
        Self(message.into())
    }

    pub(crate) fn io(context: &str, error: io::Error) -> Self {
        Self::new(format!("{context}: {error}"))
    }
}

impl fmt::Display for StoreError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(&self.0)
    }
}

impl error::Error for StoreError {}

pub type Result<T> = std::result::Result<T, StoreError>;
