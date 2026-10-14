use crate::build::{build, BuildOutcome};
use crate::error::{Result, WarpError};
use std::ffi::OsString;
use std::path::PathBuf;

#[derive(Debug, PartialEq, Eq)]
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
    fn parse(value: &str) -> Result<Self> {
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

impl BuildRequest {
    fn parse(args: impl Iterator<Item = OsString>) -> Result<Self> {
        let mut args = args;
        match args.next().as_deref().and_then(|arg| arg.to_str()) {
            Some("build") => {}
            Some("--help" | "-h") => return Err(WarpError::new(usage())),
            _ => return Err(WarpError::new(usage())),
        }

        let entry = args
            .next()
            .map(PathBuf::from)
            .ok_or_else(|| WarpError::new(usage()))?;
        let mut output = None;
        let mut cache = None;
        let mut edition = Edition::E2021;

        while let Some(argument) = args.next() {
            let option = argument
                .to_str()
                .ok_or_else(|| WarpError::new("CLI options must be valid UTF-8"))?;
            match option {
                "--output" => output = Some(PathBuf::from(required_value(&mut args, option)?)),
                "--cache" => cache = Some(PathBuf::from(required_value(&mut args, option)?)),
                "--edition" => {
                    let value = required_value(&mut args, option)?;
                    let value = value
                        .to_str()
                        .ok_or_else(|| WarpError::new("Rust edition must be valid UTF-8"))?;
                    edition = Edition::parse(value)?;
                }
                _ => return Err(WarpError::new(format!("unknown option {option:?}"))),
            }
        }

        let crate_name = crate_name(&entry)?;
        let output = output.unwrap_or_else(|| {
            PathBuf::from("target")
                .join("warp")
                .join(executable_name(&crate_name))
        });
        let cache = cache.unwrap_or_else(|| {
            output
                .parent()
                .unwrap_or_else(|| std::path::Path::new("."))
                .join(".warp-cache")
        });

        Ok(Self {
            entry,
            output,
            cache,
            edition,
        })
    }
}

fn required_value(args: &mut impl Iterator<Item = OsString>, option: &str) -> Result<OsString> {
    args.next()
        .ok_or_else(|| WarpError::new(format!("{option} requires a value")))
}

pub fn crate_name(entry: &std::path::Path) -> Result<String> {
    let stem = entry
        .file_stem()
        .and_then(|stem| stem.to_str())
        .ok_or_else(|| WarpError::new("Rust entry point must have a UTF-8 file name"))?;
    let name = stem.replace('-', "_");
    let mut characters = name.chars();
    let valid_start = characters
        .next()
        .is_some_and(|character| character == '_' || character.is_ascii_alphabetic());
    if !valid_start
        || !characters.all(|character| character == '_' || character.is_ascii_alphanumeric())
    {
        return Err(WarpError::new(format!(
            "entry point {stem:?} does not produce a valid Rust crate name"
        )));
    }
    Ok(name)
}

fn executable_name(crate_name: &str) -> String {
    if cfg!(windows) {
        format!("{crate_name}.exe")
    } else {
        crate_name.to_string()
    }
}

fn usage() -> String {
    "usage: warp build <main.rs> [--edition 2015|2018|2021|2024] [--output PATH] [--cache DIR]"
        .to_string()
}

pub fn run(args: impl Iterator<Item = OsString>) -> Result<String> {
    let request = BuildRequest::parse(args)?;
    let outcome = build(&request)?;
    let status = match outcome {
        BuildOutcome::Compiled { key } => format!("built ({})", &key[..12]),
        BuildOutcome::CacheHit { key } => format!("cached ({})", &key[..12]),
    };
    Ok(format!("{status}: {}", request.output.display()))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn args(values: &[&str]) -> impl Iterator<Item = OsString> {
        values
            .iter()
            .map(|value| OsString::from(*value))
            .collect::<Vec<_>>()
            .into_iter()
    }

    #[test]
    fn parses_build_request_and_defaults() {
        let request = BuildRequest::parse(args(&["build", "src/main.rs"])).unwrap();
        assert_eq!(request.entry, PathBuf::from("src/main.rs"));
        assert_eq!(request.output, PathBuf::from("target/warp/main"));
        assert_eq!(request.cache, PathBuf::from("target/warp/.warp-cache"));
        assert_eq!(request.edition, Edition::E2021);
    }

    #[test]
    fn parses_explicit_options() {
        let request = BuildRequest::parse(args(&[
            "build",
            "src/main.rs",
            "--edition",
            "2024",
            "--output",
            "out/warp",
            "--cache",
            "cache",
        ]))
        .unwrap();
        assert_eq!(request.output, PathBuf::from("out/warp"));
        assert_eq!(request.cache, PathBuf::from("cache"));
        assert_eq!(request.edition, Edition::E2024);
    }

    #[test]
    fn rejects_unsupported_or_incomplete_arguments() {
        assert!(BuildRequest::parse(args(&["build", "main.rs", "--edition", "2017"])).is_err());
        assert!(BuildRequest::parse(args(&["build", "main.rs", "--cache"])).is_err());
        assert!(BuildRequest::parse(args(&["build", "main.rs", "--mystery"])).is_err());
        assert!(BuildRequest::parse(args(&["test"])).is_err());
    }

    #[test]
    fn derives_valid_crate_names() {
        assert_eq!(
            crate_name(std::path::Path::new("my-app.rs")).unwrap(),
            "my_app"
        );
        assert!(crate_name(std::path::Path::new("bad-name.rs")).is_ok());
        assert!(crate_name(std::path::Path::new("123.rs")).is_err());
        assert!(crate_name(std::path::Path::new("non-rust-name!.rs")).is_err());
    }
}
