use crate::error::{Result, WarpError};
use std::path::PathBuf;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BuildRequest {
    pub entry: PathBuf,
    pub output: PathBuf,
    pub cache: PathBuf,
    pub edition: Edition,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Edition {
    E2015,
    E2018,
    E2021,
    E2024,
}

impl Edition {
    pub fn parse(value: &str) -> Result<Self> {
        match value {
            "2015" => Ok(Self::E2015),
            "2018" => Ok(Self::E2018),
            "2021" => Ok(Self::E2021),
            "2024" => Ok(Self::E2024),
            _ => Err(WarpError::new(format!(
                "unsupported Rust edition {value:?}; expected 2015, 2018, 2021, or 2024"
            ))),
        }
    }

    pub fn as_str(self) -> &'static str {
        match self {
            Self::E2015 => "2015",
            Self::E2018 => "2018",
            Self::E2021 => "2021",
            Self::E2024 => "2024",
        }
    }
}
