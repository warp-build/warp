use std::{error, fmt, io};

#[derive(Debug)]
pub struct WarpError(String);

impl WarpError {
    pub fn new(message: impl Into<String>) -> Self {
        Self(message.into())
    }

    pub fn io(context: &str, error: io::Error) -> Self {
        Self::new(format!("{context}: {error}"))
    }
}

impl fmt::Display for WarpError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(&self.0)
    }
}

impl error::Error for WarpError {}

pub type Result<T> = std::result::Result<T, WarpError>;
