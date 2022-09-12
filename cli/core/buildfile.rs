use super::*;
use std::path::PathBuf;
use thiserror::*;
use tokio::fs;

pub const BUILDFILE: &str = "Build.toml";

#[derive(Debug)]
pub struct Buildfile {
    /// The Path to this build file.
    pub file: PathBuf,

    /// The Targets defined within this build file.
    pub targets: Vec<Target>,
}

#[derive(Error, Debug)]
pub enum BuildfileError {
    #[error(transparent)]
    FileReadError(std::io::Error),

    #[error(transparent)]
    TomlError(toml::de::Error),

    #[error("Expected contents of a Build.toml file to be a TOML table, but instead found: {0:?}")]
    ExpectedTomlTable(toml::Value),

    #[error("Rule should be table")]
    RuleShouldBeTable,

    #[error(transparent)]
    TargetError(TargetError),

    #[error(transparent)]
    RuleConfigError(RuleConfigError),

    #[error("The following target does not have a name: {config:?}")]
    TargetNeedsName {
        config: RuleConfig,
        err: RuleConfigError,
    },
}

impl Buildfile {
    #[tracing::instrument(name = "Buildfile::from_file")]
    pub async fn from_file(file: &PathBuf) -> Result<Buildfile, BuildfileError> {
        let string = fs::read_to_string(&file)
            .await
            .map_err(BuildfileError::FileReadError)?;

        let contents = string
            .parse::<toml::Value>()
            .map_err(BuildfileError::TomlError)?;

        let targets = Self::from_toml(&file, &contents).await?;

        Ok(Self {
            file: file.to_path_buf(),
            targets,
        })
    }

    pub async fn from_toml(
        path: &PathBuf,
        toml: &toml::Value,
    ) -> Result<Vec<Target>, BuildfileError> {
        let contents = toml
            .as_table()
            .ok_or_else(|| BuildfileError::ExpectedTomlTable(toml.clone()))?;

        let mut targets: Vec<Target> = vec![];

        for (rule_name, configs) in contents.iter() {
            for cfg in configs
                .as_array()
                .ok_or_else(|| BuildfileError::RuleShouldBeTable)?
            {
                let config: RuleConfig =
                    TryFrom::try_from(toml).map_err(BuildfileError::RuleConfigError)?;

                let name =
                    config
                        .get_string("name")
                        .map_err(|err| BuildfileError::TargetNeedsName {
                            config: config.clone(),
                            err,
                        })?;

                let label = Label::from_path_and_name(&path, &name);

                let target = Target::new(label, rule_name, config);

                targets.push(target);
            }
        }

        Ok(targets)
    }
}
